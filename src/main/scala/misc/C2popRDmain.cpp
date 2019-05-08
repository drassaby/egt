#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>

#define RUNS 1000
#define MAXGEN 100000
#define ATYPES 4
#define BTYPES 5
#define ERR 0.0

const double STATEBIAS = 0.5;

double popA[ATYPES];
double popB[BTYPES];
double oldpopA[ATYPES];
double oldpopB[BTYPES];
int stable, gen;
double totalsA[ATYPES];
double totalsB[BTYPES];
double payoffA[ATYPES];
double payoffB[BTYPES];
double avepayA;
double avepayB;
int sender[ATYPES][2];
int receiver[BTYPES][2];
int count;


//2-pop Signaling Game 
 const double matrixA[ATYPES][BTYPES] = 
{{1.1, 3.1, 0.6, 3.6, 3.1},
	{2.9, 0.9, 0.4, 3.4, 2.9},
	{0.5, 3.5, 0.5, 3.5, 0.5},
	{3.5, 0.5, 0.5, 3.5, 0.5}};
 
 const double matrixB[BTYPES][ATYPES] = 

{{1.0, 3.0, 2.5, 1.5},
	{3.0, 1.0, 1.5, 2.5},
	{2.5, 2.5, 2.5, 2.5},
	{1.5, 1.5, 1.5, 1.5},
	{2.5, 2.5, 2.0, 2.0}};


void DefineSigTypes(void) {
	int i, j, k, l;
	
	sender[0][0] = 0;
	sender[0][1] = 0;
	sender[1][0] = 0;
	sender[1][1] = 1;
	sender[2][0] = 1;
	sender[2][1] = 0;
	sender[3][0] = 1;
	sender[3][1] = 1;
	
	 receiver[0][0] = 0;
	 receiver[0][1] = 0;
	 receiver[1][0] = 0;
	 receiver[1][1] = 1;
	 receiver[2][0] = 1;
	 receiver[2][1] = 0;
	 receiver[3][0] = 1;
	 receiver[3][1] = 1; 
	
	return;
}


/* Get a random (flat) starting distribution */
void RandFill(void) {
	double randomA[ATYPES], yA[ATYPES];
	double randomB[BTYPES], yB[BTYPES];
	unsigned long i, j;
	double x;
	x=0;
	
	while(x==0) {
		for(i=0;i<ATYPES;i++) {
			randomA[i] = (rand()*1.0)/RAND_MAX;
			yA[i] = 0-log(randomA[i]);
			x += yA[i];
		}
		for(i=0;i<ATYPES;i++) {
			popA[i] = (yA[i] / x);
		}
	}
	
	x=0;
	while(x==0) {
		for(i=0;i<BTYPES;i++) {
			randomB[i] = (rand()*1.0)/RAND_MAX;
			yB[i] = 0-log(randomB[i]);
			x += yB[i];
		}
		for(i=0;i<BTYPES;i++) {
			popB[i] = (yB[i] / x);
		}
	}
	
	return;
} 


/* The replicator dynamics */
void Replicate(void) {
	double sum;
	int i, j, k, l;
	
	/* Save current pop to old pop */
	for(i=0;i<ATYPES;i++) {
		oldpopA[i]=popA[i];
	}
	for(i=0;i<BTYPES;i++) {
		oldpopB[i]=popB[i];
	}
	
	/* Clear Payoffs */
	avepayA = 0;
	for(i=0;i<ATYPES;i++) {
		payoffA[i] = 0;
	}
	avepayB = 0;
	for(i=0;i<BTYPES;i++) {
		payoffB[i] = 0;
	}
	
	 
	 /* Get new payoffs -- for Matrix */
	 for(i=0;i<ATYPES;i++) {
	 for(j=0;j<BTYPES;j++) {
	 payoffA[i] += (matrixA[i][j]*popB[j]);
	 }
	 avepayA += payoffA[i]*popA[i];
	 }
	 for(i=0;i<BTYPES;i++) {
	 for(j=0;j<ATYPES;j++) {
	 payoffB[i] += (matrixB[i][j]*popA[j]);
	 }
	 avepayB += payoffB[i]*popB[i];
	 } 
	
	// Replicate
    for(i=0;i<ATYPES;i++) {
        popA[i] = popA[i]*(payoffA[i]/avepayA);
    }
	for(i=0;i<BTYPES;i++) {
        popB[i] = popB[i]*(payoffB[i]/avepayB);
    }
	
	return;
}


/* Detect a stable state */
void DetectStable(void) {
	int i, j;
	j=0;
	
	for(i=0;i<ATYPES;i++) {
		if(oldpopA[i]==popA[i]) j++;
	}
	for(i=0;i<BTYPES;i++) {
		if(oldpopB[i]==popB[i]) j++;
	}
	
	if(j==ATYPES+BTYPES) stable = 1;
	else stable = 0;
	
	return;
}


/* Print results of run */
void PrintRun(void) {
	int i;
	
	//printf(" \n");
	
	for(i=0;i<ATYPES;i++) {
		printf("%f ", popA[i]);
		totalsA[i] += popA[i];
	}
	
	printf("      ");
	
	for(i=0;i<BTYPES;i++) {
		printf("%f ", popB[i]);
		totalsB[i] += popB[i];
	}
	
	printf(" Gen: %d \n", gen);
	
	//printf(" \n");
	
	if((popA[1]==1 && popB[1]==1) || (popA[2]==1 && popB[2]==1)) count++;
	
	return;
}


/* PrintEnd */
void PrintEnd(void) {
	printf("Proportion of Signaling Systems: %f \n", count*1.0/RUNS);
	
	return;
} 

/* Main program */
int main(void) {
	unsigned long i;
	printf("2 Population Replicator Dynamics\n");
	srand( (unsigned int) time(NULL));
	DefineSigTypes();
	count = 0;
	for(i=0;i<RUNS;i++) {
		gen=0;
		stable = 0;
		RandFill();
		while(stable!=1 && gen<=MAXGEN){
			Replicate();
			DetectStable();
			gen++;
			//if(gen%20==0) PrintRun();
		}
		PrintRun();
		//if(i%100==0) printf("Run: %d x100 \n", i/100);
	}
	PrintEnd();
}
