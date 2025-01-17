-- | ignore

import "lu"

module lu64 = mk_lu f64

entry test_lu = lu64.lu -- to make sure small blocks work.

-- ==
-- entry: test_lu
-- input { 1i64 [[1.0,-1.0,3.0],[2.0,-3.0,1.0],[3.0,2.0,1.0]] }
-- output { [[1.0,-1.0,3.0],[2.0,-1.0,-5.0],[3.0,-5.0,-33.0]] }
-- input { 16i64
-- [
-- [0.212371, 0.413407, 0.387983, 0.848766, 0.077641, 0.562286, 0.162389, 0.712271, 0.868543, 0.239492, 0.182439, 0.171670, 0.445346, 0.687744, 0.039794, 0.306682],
-- [0.274594, 0.870336, 0.258884, 0.385665, 0.843955, 0.580725, 0.579602, 0.410075, 0.657208, 0.895538, 0.228201, 0.808103, 0.085870, 0.161693, 0.463080, 0.487156],
-- [0.575100, 0.851063, 0.335922, 0.652740, 0.413350, 0.498311, 0.365012, 0.281892, 0.737804, 0.547450, 0.453562, 0.183149, 0.235194, 0.493356, 0.489831, 0.509789],
-- [0.363692, 0.748715, 0.895453, 0.207647, 0.329440, 0.617723, 0.720832, 0.729691, 0.190043, 0.293211, 0.734553, 0.483644, 0.956780, 0.523890, 0.795065, 0.458524],
-- [0.350162, 0.445601, 0.869949, 0.493129, 0.617956, 0.427524, 0.805978, 0.322211, 0.186951, 0.133932, 0.053546, 0.809193, 0.585701, 0.541825, 0.820578, 0.715656],
-- [0.811360, 0.433218, 0.011626, 0.481710, 0.445907, 0.371511, 0.552982, 0.906577, 0.120456, 0.000957, 0.082731, 0.311845, 0.273010, 0.640507, 0.082937, 0.647688],
-- [0.355638, 0.969212, 0.822457, 0.796161, 0.454932, 0.428366, 0.048996, 0.541793, 0.216726, 0.131932, 0.360726, 0.486796, 0.491562, 0.893656, 0.934112, 0.895783],
-- [0.558085, 0.581800, 0.222933, 0.913723, 0.675394, 0.844002, 0.882935, 0.140499, 0.546004, 0.405129, 0.512182, 0.368461, 0.928280, 0.966595, 0.502763, 0.724441],
-- [0.457840, 0.712595, 0.179373, 0.589593, 0.140961, 0.228369, 0.131385, 0.357687, 0.360302, 0.492111, 0.844483, 0.851864, 0.360394, 0.857575, 0.035788, 0.740510],
-- [0.145638, 0.688469, 0.729046, 0.073918, 0.655064, 0.798359, 0.944404, 0.977732, 0.702497, 0.085365, 0.206101, 0.833882, 0.711760, 0.136492, 0.462312, 0.399335],
-- [0.272514, 0.271611, 0.095037, 0.216918, 0.797534, 0.302283, 0.455444, 0.631416, 0.745335, 0.957409, 0.669169, 0.606026, 0.324781, 0.525524, 0.742518, 0.837968],
-- [0.987836, 0.693592, 0.953503, 0.535388, 0.485206, 0.828206, 0.837671, 0.940650, 0.459622, 0.240399, 0.248949, 0.449995, 0.495951, 0.814421, 0.031727, 0.774481],
-- [0.516933, 0.457583, 0.770486, 0.420079, 0.479339, 0.386361, 0.831323, 0.341635, 0.582476, 0.225469, 0.232711, 0.193558, 0.137397, 0.442568, 0.460872, 0.582961],
-- [0.273891, 0.924596, 0.662860, 0.494465, 0.883587, 0.228942, 0.360803, 0.333969, 0.820195, 0.128235, 0.121557, 0.045480, 0.787698, 0.679195, 0.160920, 0.743881],
-- [0.323838, 0.442513, 0.668477, 0.162942, 0.081946, 0.758564, 0.361896, 0.880121, 0.826626, 0.241131, 0.122968, 0.209200, 0.672339, 0.114852, 0.975380, 0.220268],
-- [0.010140, 0.815826, 0.890262, 0.123261, 0.056956, 0.321195, 0.024272, 0.999652, 0.153927, 0.328611, 0.137974, 0.883150, 0.242625, 0.695274, 0.627997, 0.132887]
-- ]
-- }
-- output {
-- [
-- [0.212371, 0.413407, 0.387983, 0.848766, 0.077641, 0.562286, 0.162389, 0.712271, 0.868543, 0.239492, 0.182439, 0.171670, 0.445346, 0.687744, 0.039794, 0.306682],
-- [1.292992, 0.335804, -0.242775, -0.711783, 0.743566, -0.146306, 0.369634, -0.510886, -0.465811, 0.585877, -0.007691, 0.586135, -0.489959, -0.727555, 0.411627, 0.090619],
-- [2.707997, -0.799400, -0.908809, -2.214715, 0.797505, -1.141315, 0.220749, -2.055337, -1.986577, 0.367256, -0.046631, 0.186824, -1.362475, -1.950660, 0.711123, -0.248264],
-- [1.712531, 0.121328, -0.286612, -1.794296, 0.334836, -0.654573, 0.461158, -1.017195, -1.810225, -0.082750, 0.409689, 0.172085, -0.136944, -1.124702, 0.880791, -0.148829],
-- [1.648822, -0.702891, -0.065569, 0.864884, 0.775283, -0.111129, 0.413666, -0.466305, -0.137160, 0.246510, -0.610060, 0.801545, -0.463878, -0.258700, 0.329139, 0.386128],
-- [3.820484, -3.413291, 2.530035, -0.230061, 0.963009, 0.567913, 0.343475, 1.856670, -0.046014, -0.099851, 0.159198, 0.451656, 0.761523, 0.455254, -0.577589, 0.007355],
-- [1.674607, 0.824641, -0.410361, 0.527815, -0.177648, -0.942384, -0.283405, 1.130634, -0.781095, -0.608182, -0.132171, 0.269828, 0.298236, 0.518152, -0.130888, 0.359685],
-- [2.627878, -1.502611, 1.277975, -0.247500, 0.841414, 0.944885, -0.603482, -0.803677, -0.658078, -0.313803, 0.465315, -0.336457, 0.579805, 0.380794, 0.515726, 0.220344],
-- [2.155850, -0.532002, 0.865107, -0.165567, -0.342239, -0.388650, -0.488313, -1.585374, -1.830615, -0.792847, 1.081495, 0.708670, 1.497698, 1.434276, 0.341485, 0.977666],
-- [0.685772, 1.205960, -0.831589, 0.831241, 0.116056, 0.347059, -0.071639, 0.332890, -0.391312, -0.654537, -0.014678, 0.180486, 0.182004, 0.198641, -0.087341, 0.284622],
-- [1.283198, -0.770902, 0.649176, -0.009369, 0.975820, 0.547963, 0.701616, 0.880069, -1.000018, -0.896419, 1.722900, 0.665531, 1.235545, 1.254606, 0.451858, 1.079790],
-- [4.651464, -3.660931, 1.914559, 0.991030, 1.273732, 1.148943, 1.290240, 2.858967, -1.872882, -0.989736, 0.290666, 1.655389, -0.325524, 0.368541, -1.520221, 0.507106],
-- [2.434103, -1.633966, 0.627845, 0.790526, 0.954372, 0.209050, -0.247960, -0.254976, -0.089899, 0.125007, 0.124994, -0.265936, -0.407327, 0.116145, -0.619646, 0.085582],
-- [1.289682, 1.165658, -0.490178, 0.477113, 0.190738, -0.971330, 0.481952, -1.040456, 0.105894, 1.126744, 0.230866, -0.891648, -1.302339, 0.673288, -2.670574, 0.071550],
-- [1.524869, -0.559489, 0.064894, 0.772352, 0.089265, 0.719899, 1.177164, 2.735869, -1.930447, -0.515770, 0.249386, 0.985669, -1.146448, 0.038899, 0.941524, 0.228716],
-- [0.047747, 2.370690, -1.592503, 0.979097, -0.989733, -1.136531, 0.564409, -1.132423, 0.364308, -0.002082, -0.315310, 0.205282, -0.375700, 0.926210, 2.993256, -0.747126]
-- ]
-- }

entry test_lu2_l = \b m -> (lu64.lu2 b m).0 -- to make sure small blocks work.

-- ==
-- entry: test_lu2_l
-- input { 1i64 [[1.0,-1.0,3.0],[2.0,-3.0,1.0],[3.0,2.0,1.0]] }
-- output { [[1.0,0.0,0.0],[2.0,1.0,0.0],[3.0,-5.0,1.0]] }
-- input { 16i64 [[1.0,-1.0,3.0],[2.0,-3.0,1.0],[3.0,2.0,1.0]] }
-- output { [[1.0,0.0,0.0],[2.0,1.0,0.0],[3.0,-5.0,1.0]] }

entry test_lu2_u = \b m -> (lu64.lu2 b m).1 -- to make sure small blocks work.

-- ==
-- entry: test_lu2_u
-- input { 1i64 [[1.0,-1.0,3.0],[2.0,-3.0,1.0],[3.0,2.0,1.0]] }
-- output { [[1.0,-1.0,3.0],[0.0,-1.0,-5.0],[0.0,0.0,-33.0]] }
-- input { 16i64 [[1.0,-1.0,3.0],[2.0,-3.0,1.0],[3.0,2.0,1.0]] }
-- output { [[1.0,-1.0,3.0],[0.0,-1.0,-5.0],[0.0,0.0,-33.0]] }

entry test_forsolve = lu64.forsolve

-- ==
-- entry: test_forsolve
-- input {
--   [[2f64,0f64,0f64],[3f64,1f64,0f64],[1f64,4f64,5f64]]
--   [2f64,-1f64,8f64]
-- }
-- output { [1f64,-4f64,4.6f64] }
-- input {
--   [[1f64,0f64,0f64],[-1f64,2f64,0f64],[3f64,4f64,5f64]]
--   [1f64,9f64,-2f64]
-- }
-- output { [1f64,5f64,-5f64] }
-- input {
--   [[1f64,200f64,300f64],[-1f64,2f64,400f64],[3f64,4f64,5f64]]
--   [1f64,9f64,-2f64]
-- }
-- output { [1f64,5f64,-5f64] }

entry test_backsolve = lu64.backsolve

-- ==
-- entry: test_backsolve
-- input {
--   [[1f64,-1f64,3f64],[0f64,2f64,9f64],[0f64,0f64,1f64]]
--   [1f64,9f64,-2f64]
-- }
-- output { [20.5f64,13.5f64,-2f64] }
-- input {
--   [[1f64,-1f64,3f64],[400f64,2f64,9f64],[800f64,900f64,1f64]]
--   [1f64,9f64,-2f64]
-- }
-- output { [20.5f64,13.5f64,-2f64] }

entry test_ols = lu64.ols

-- ==
-- entry: test_ols
-- input {
--   1i64
--   [[1f64, 4f64, 5f64], [6f64, 8f64, 22f64], [32f64, 5f64, 5f64]]
--   [1f64, 2f64, 3f64]
-- }
-- output { [0.05614973f64, 0.25935829f64, -0.01871658f64] }
