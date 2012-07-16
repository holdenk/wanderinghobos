#include <stdint.h>
#ifndef C_SIMULATE_H
#define C_SIMULATE_H

struct cboard
{
	int width, height;
	uint8_t* board;
};

typedef struct cboard cboard;

extern cboard * make_board(int width, int height, uint8_t * tehboard);

struct point
{
	int x, y;
};

typedef struct point point;

struct sim_board_info {
  int* falling_rocks;
  int falling_rocks_count;
  int moving_rocks;
};

typedef struct sim_board_info sim_board_info;

struct cworld {
  cboard* board;
  int water;
  int flooding;
  int waterproof;
  int underwater;
  int iteration;
  int * rocks;
  int fallen_rock_count;
  int robot_x;
  int robot_y;
  int * hugs;
  int hug_count;
  int lift_x;
  int lift_y;
  int fuckedrocks;
  int beard;
  int razors;
};

typedef struct cworld cworld;

extern cworld * make_cworld(cboard* board, int water, int flooding, int waterproof,
			    int underwater, int iteration, int robot_x,
			    int robot_y, int lift_x, int lift_y, int fuckedrocks,
			    int beard, int razors);

extern point * make_point(int x, int y);

#define APT(x,y,w) (y*w+x)
#define CELL_BOARDPTR(b,x,y) (b->board[APT(x,y,b->width)])
#define IS_ROCK_LIKE(a) (a == '*' || a == '@')
#define IS_EMPTY(a) (a == ' ')
#define IS_HUG(a) (a == '\\')
#define IS_CLOSED_LIFT(a) (a == 'L')
#define IS_BEARD(a) (a == 'W')
#define CONVERTPOINT(x,y,in) reversedX*in->height-x,y

extern point * native_execute_square(cboard* in,
				   point* pt,
				   cboard* out,
				   int hugs,
				   int beard);
extern sim_board_info * native_simulate_board(cboard *in, cboard *out, int nr_hugs, int grow_beard);

#endif
