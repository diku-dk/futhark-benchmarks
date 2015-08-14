INCLUDES  += -I/usr/local/cuda/include
GPU_OPTS   = -D lgWARP=5 -L/usr/local/cuda/lib64

SOURCES_CPP =test.cpp
EXECUTABLE  =testBB

default: gpu

gpu: 
	$(CXX) $(CXXFLAGS) -D IS_GPU=1 $(GPU_OPTS) $(INCLUDES) -o $(EXECUTABLE) $(SOURCES_CPP) -lOpenCL


run: 
	./testBB

clean:
	rm -f $(EXECUTABLE) ./*.ptx
	@# clean nVidia compiler cache
	rm -rf $(HOME)/.nv/ComputeCache/*

