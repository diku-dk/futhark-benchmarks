#include <iostream>
#include <fstream>
#include <cmath>
#include <math.h>

#define GAMMA 1.4
#define iterations 2000

#define NDIM 3
#define NNB 4

#define RK 3	// 3rd order RK
#define ff_mach 1.2
#define deg_angle_of_attack 0.0f

struct float3 { float x, y, z; };

//#define DEBUG

/*
 * not options
 */
#define VAR_DENSITY 0
#define VAR_MOMENTUM  1
#define VAR_DENSITY_ENERGY (VAR_MOMENTUM+NDIM)
#define NVAR (VAR_DENSITY_ENERGY+1)

template<class T>
void print1Darray(std::ofstream& file, int d, T* arr){
    file << "[ ";
    file << arr[0];
    for(int i=1; i<d; i++) {
        file << ", ";
        file << arr[i];
    }
    file << " ]";
}

#if 0
int main() { 
    std::ofstream file("lala"); 
    int* arr; 
    print1Darray<int>(file, 3, arr); 
    return 1; 
}
#endif

template<class T>
void print2Darray(std::ofstream& file, int d1, int d2, T* arr){
    file << "[ "; 
    print1Darray(file, d2, arr);
    for(int i=1; i<d1; i++) {
        arr += d2;
        file << ", "; 
        print1Darray(file, d2, arr);
    }
    file << " ]";
}

inline void compute_flux_contribution(float& density, float3& momentum, float& density_energy, float& pressure, float3& velocity, float3& fc_momentum_x, float3& fc_momentum_y, float3& fc_momentum_z, float3& fc_density_energy)
{
	fc_momentum_x.x = velocity.x*momentum.x + pressure;
	fc_momentum_x.y = velocity.x*momentum.y;
	fc_momentum_x.z = velocity.x*momentum.z;

	fc_momentum_y.x = fc_momentum_x.y;
	fc_momentum_y.y = velocity.y*momentum.y + pressure;
	fc_momentum_y.z = velocity.y*momentum.z;

	fc_momentum_z.x = fc_momentum_x.z;
	fc_momentum_z.y = fc_momentum_y.z;
	fc_momentum_z.z = velocity.z*momentum.z + pressure;

	float de_p = density_energy+pressure;
	fc_density_energy.x = velocity.x*de_p;
	fc_density_energy.y = velocity.y*de_p;
	fc_density_energy.z = velocity.z*de_p;
}

int main(int argc, char** argv)
{
	if (argc < 3)
	{
		std::cout << "specify input and output data file names" << std::endl;
		return 0;
	}
	const char* inp_file_name = argv[1];
    const char* out_file_name = argv[2];

        float ff_variable[NVAR];
        float3 ff_flux_contribution_momentum_x, ff_flux_contribution_momentum_y, ff_flux_contribution_momentum_z, ff_flux_contribution_density_energy;

	// set far field conditions
	{
		const float angle_of_attack = float(3.1415926535897931 / 180.0f) * float(deg_angle_of_attack);

		ff_variable[VAR_DENSITY] = float(1.4);

		float ff_pressure = float(1.0f);
		float ff_speed_of_sound = sqrt(GAMMA*ff_pressure / ff_variable[VAR_DENSITY]);
		float ff_speed = float(ff_mach)*ff_speed_of_sound;

		float3 ff_velocity;
		ff_velocity.x = ff_speed*float(cos((float)angle_of_attack));
		ff_velocity.y = ff_speed*float(sin((float)angle_of_attack));
		ff_velocity.z = 0.0f;

		ff_variable[VAR_MOMENTUM+0] = ff_variable[VAR_DENSITY] * ff_velocity.x;
		ff_variable[VAR_MOMENTUM+1] = ff_variable[VAR_DENSITY] * ff_velocity.y;
		ff_variable[VAR_MOMENTUM+2] = ff_variable[VAR_DENSITY] * ff_velocity.z;

		ff_variable[VAR_DENSITY_ENERGY] = ff_variable[VAR_DENSITY]*(float(0.5f)*(ff_speed*ff_speed)) + (ff_pressure / float(GAMMA-1.0f));

		float3 ff_momentum;
		ff_momentum.x = *(ff_variable+VAR_MOMENTUM+0);
		ff_momentum.y = *(ff_variable+VAR_MOMENTUM+1);
		ff_momentum.z = *(ff_variable+VAR_MOMENTUM+2);
		compute_flux_contribution(ff_variable[VAR_DENSITY], ff_momentum, ff_variable[VAR_DENSITY_ENERGY], ff_pressure, ff_velocity, ff_flux_contribution_momentum_x, ff_flux_contribution_momentum_y, ff_flux_contribution_momentum_z, ff_flux_contribution_density_energy);
	}
	int nel;
	int nelr;


	// read in domain geometry
	float* areas;
	int* elements_surrounding_elements;
	float* normals;
	{
		std::ifstream file(inp_file_name);

		file >> nel;
#ifdef DEBUG
        nel  = 32; // cosmin: for debug purposes
#endif

		nelr = nel; //block_length*((nel / block_length )+ std::min(1, nel % block_length));

		areas = new float[nelr];                           // [nelr]          -> [nelr]
		elements_surrounding_elements = new int[nelr*NNB]; // [NNB,nelr]      -> [nelr, NNB]
		normals = new float[NDIM*NNB*nelr];                // [NDIM,NNB,nelr] -> [nelr, NNB, NDIM]

		// read in data
		for(int i = 0; i < nel; i++)
		{
			file >> areas[i];
			for(int j = 0; j < NNB; j++) // COSMIN: NNB == 4
			{
				file >> elements_surrounding_elements[i + j*nelr];
				if(elements_surrounding_elements[i+j*nelr] < 0) elements_surrounding_elements[i+j*nelr] = -1;
				elements_surrounding_elements[i + j*nelr]--; //it's coming in with Fortran numbering

#ifdef DEBUG
                if(elements_surrounding_elements[i+j*nelr] > 0) {
                    elements_surrounding_elements[i+j*nelr] = 
                        elements_surrounding_elements[i+j*nelr] % nel;
                }
#endif

				for(int k = 0; k < NDIM; k++) // COSMIN: NDIM == 3
				{
					file >>  normals[i + (j + k*NNB)*nelr];
					normals[i + (j + k*NNB)*nelr] = -normals[i + (j + k*NNB)*nelr];
				}
			}
		}

		// fill in remaining data
		int last = nel-1;
		for(int i = nel; i < nelr; i++)
		{
			areas[i] = areas[last];
			for(int j = 0; j < NNB; j++)
			{
				// duplicate the last element
				elements_surrounding_elements[i + j*nelr] = elements_surrounding_elements[last + j*nelr];
				for(int k = 0; k < NDIM; k++) normals[i + (j + k*NNB)*nelr] = normals[last + (j + k*NNB)*nelr];
			}
		}
	}

    // write out arrays areas, elements_surrounding_elements, and normals in tuple of arrays form
    {
        std::ofstream file(out_file_name);
        //file << nel << " -- number of elements\n\n";

        // areas[nelr]
        file << "[\t" << areas[0];
        for(int i=1; i<nelr; i+=8) {
            file << "\n\t";
            for(int t=i; t<std::min(i+8,nelr); t++) {
                file << ", " << areas[t];
            }
        }
        file << "\n] -- END ARRAY areas[nel]!\n\n";

        // elements_surrounding_elements[NNB, nelr]
        int* int_arr = elements_surrounding_elements;
        file << "[\t";
        print1Darray<int>(file, nelr, int_arr);
        for(int i=1; i<NNB; i++) {
            int_arr += nelr;
            file << "\n\t, ";
            print1Darray<int>(file, nelr, int_arr);
        }
        file << "\n] -- END ARRAY elements_surrounding_elements[NNB,nel]!\n\n";


        // normals[NDIM, NNB, nelr]
        float* float_arr = normals;
        file << "[ ";
        print2Darray<float>(file, NNB, nelr, float_arr);
        for(int i=1; i<NDIM; i++) {
            float_arr += NNB*nelr;
            file << "\n\t, ";
            print2Darray<float>(file, NNB, nelr, float_arr);
        }
        file << "\n] -- END ARRAY normals[NDIM, NNB, nel]!\n\n";
    }

    return 1;
}

