set -x
source ../versions/build.ver
module reset
module use `pwd`
module load cdas.module.lua
module list

make clean
make
