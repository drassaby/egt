#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>

//Number of Simulations
#define RUNS 10

//Number of Maximum generations per simulation
#define MAXGEN 10000

//This Sets the number of strategies in the game
#define TYPES 3

//Mutation Probability (set to zero for no-mutation)
const double mu = 0.001;

double pop[TYPES];
double oldpop[TYPES];
int stable, gen;
double totals[TYPES];
double results[MAXGEN][TYPES];


//Payoff Matrix (3 strategies)
//Symmetric games only -- payoffs listed are only for player 1
const double matrix[TYPES][TYPES] = 
	{{4,1,1},
	 {3,0,0},
	 {3,0,2}};



// Get a random (flat) starting distribution
void RandFill(void) {
	double random[TYPES], y[TYPES];
	unsigned long i, j;
	double x;
	x=0;
	
	while(x==0) {
		for(i=0;i<TYPES;i++) {
			random[i] = (rand()*1.0)/RAND_MAX;
			
			//The negative logarithm is needed to ensure an unbaised distribution
			//Seems a bit strange if you don't know about random-point-picking
			//in Simplexes, but trust me...
			y[i] = 0-log(random[i]);
			x += y[i];
		}
		for(i=0;i<TYPES;i++) {
			pop[i] = (y[i] / x);
		}
	}
	
	return;
} 


// The discrete-time replicator dynamics with mutation
void Replicate(void) {
	double fitness[TYPES], avefit;
	int i, j;
	
	/* Save current pop to old pop */
	for(i=0;i<TYPES;i++) {
		oldpop[i]=pop[i];
	}
	
	/* Clear Payoffs */
	avefit = 0;
	for(i=0;i<TYPES;i++) {
		fitness[i] = 0;
	}
	
	/* Get new payoffs */
	for(i=0;i<TYPES;i++) {
		for(j=0;j<TYPES;j++) {
			fitness[i] += (matrix[i][j]*pop[j]);
		}
		avefit += fitness[i]*pop[i];
	}
	
	/* Replicate */
	for(i=0;i<TYPES;i++) {
		pop[i] = pop[i]*((1-mu)*fitness[i]/avefit)+mu/TYPES;
	}
	
	return;
}


// Detect a stable state
void DetectStable(void) {
	int i, j;
	j=0;
	
	for(i=0;i<TYPES;i++) {
		if(oldpop[i]==pop[i]) j++;
	}
	
	if(j==TYPES) stable = 1;
	else stable = 0;
	
	return;
}


// Print results of run
void PrintRun(void) {
	int i;
	
	for(i=0;i<TYPES;i++) {
		printf("%f ", pop[i]);
		totals[i] += pop[i];
	}
	
	printf(" Gen: %d \n", gen);
	
	return;
}


void StoreResults(int i){
	int j;
	
	for(j=0;j<TYPES;j++){
		results[i][j]+=(pop[j]*(1.0/RUNS));
	}
	
	return;
}

void PrintResults(void) {
	int i, j;
	
	printf("\n");
	
	printf("Mean Proportions Across Simulations: \n");
	for(i=0;i<TYPES;i++) {
			printf("%f ", results[j][i]);
	}
	printf("\n");
	
	return;
}


// Main program
int main(void) {
	unsigned long i, j;
	printf("Replicator Dynamics\n");
	srand( (unsigned int) time(NULL));
	
	for(i=0;i<TYPES;i++){
		for(j=0;j<MAXGEN;j++){
			results[j][i] = 0.0;
		}
	}
	
	for(i=0;i<RUNS;i++) {
		gen=0;
		stable = 0;
		RandFill();
		while(stable!=1 && gen<=MAXGEN){
			Replicate();
			DetectStable();
			StoreResults(gen);
			gen++;
		}
		
		PrintRun();
	}
	
	
	PrintResults();
}

