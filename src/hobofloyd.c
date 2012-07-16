#include "hobofloyd.h"

void tehsexyfloyd(int elementcount, unsigned int * vec) {
  int i,j,k,newcost;
  for (i=0; i < elementcount; ++i) {
    for (j=0; j < elementcount; ++j) {
      for (k=0; k < elementcount; ++k) {
	newcost = vec[(i*elementcount)+k]+vec[(k*elementcount)+j];
	if (vec[(i*elementcount)+j] > newcost) {
	  vec[(i*elementcount)+j] = newcost;
	}
      }
    }
  }
}
