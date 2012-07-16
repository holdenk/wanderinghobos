#ifndef C_SIMULATE_H
#define C_SIMULATE_H

struct board
{
	int width, height;
	char* board;
};

typedef struct board board;

extern board * make_board(int width, int height, signed char * tehboard);

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

extern struct point native_execute_square(struct board* in,
				   struct point pt,
				   struct board* out,
				   int hugs,
				   char beard);
#endif
