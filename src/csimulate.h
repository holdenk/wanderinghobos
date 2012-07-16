#ifndef C_SIMULATE_H
#define C_SIMULATE_H

struct cboard
{
	int width, height;
	char* board;
};

typedef struct cboard cboard;

extern cboard * make_board(int width, int height, signed char * tehboard);

struct point
{
	int x, y;
};

typedef struct point point;

extern point * make_point(int x, int y);

#define APT(x,y,w) (y*w+x)
#define CELL_BOARDPTR(b,x,y) (b->board[APT(x,y,b->width)])
#define IS_ROCK_LIKE(a) (a == '*' || a == '@')
#define IS_EMPTY(a) (a == ' ')
#define IS_HUG(a) (a == '\\')
#define IS_CLOSED_LIFT(a) (a == 'L')
#define IS_BEARD(a) (a == 'W')

extern point* native_execute_square(cboard* in,
				   point* pt,
				   cboard* out,
				   int hugs,
				   int beard);
#endif
