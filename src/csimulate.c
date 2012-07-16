#include <stdint.h>
#include "csimulate.h"

#define NULL 0

cworld * make_cworld(cboard* board, int water, int flooding, int waterproof,
		     int underwater, int iteration, int robot_x,
		     int robot_y, int lift_x, int lift_y, int fuckedrocks,
		     int beard, int razors) {
  cworld * w = (cworld *)malloc (sizeof(cworld));
  w->water = water;
  w->flooding = flooding;
  w->waterproof = waterproof;
  w->underwater = underwater;
  w->iteration = iteration;
  w->rocks = NULL;
  w->robot_x = robot_x;
  w->robot_y = robot_y;
  w->hugs = NULL;
  w->lift_x = lift_x;
  w->lift_y = lift_y;
  w->fuckedrocks = fuckedrocks;
  w->beard = beard;
  w->razors = razors;
  w->hug_count = -1;
  w->fallen_rock_count = -1;
}

sim_board_info * native_simulate_board(cboard *in, cboard * out, int nr_hugs, int grow_beard) {
  int y,x;
  memcpy(out->board,in->board,in->width*in->height*sizeof(char));
  sim_board_info * r = malloc(sizeof(sim_board_info));
  r->falling_rocks_count = 0;
  r->falling_rocks = 0;
  //TODO MAYBE: fix!
  r->moving_rocks = 1;
  point * pt = malloc(sizeof(point));
  for (y = 0; y < in->height ; ++y) {
    for (x = 0; x < in->width ; ++x) {
      pt->x = x;
      pt->y = y;
      point * p = native_execute_square(in,pt,out,nr_hugs,grow_beard);
      if (p != 0) {
	r->falling_rocks_count++;
	r->falling_rocks = realloc(r->falling_rocks,sizeof(int)*2*r->falling_rocks_count);
	r->falling_rocks[2*r->falling_rocks_count-2] = p->x;
	r->falling_rocks[2*r->falling_rocks_count-1] = p->y;
	free(p);
      }
    }
  }
}
void * native_simulate_world(cworld* in, cworld* out) {
  //TODO
}

cboard * make_board(int width, int height, uint8_t * tehboard) {
  cboard *b = (cboard *)malloc (sizeof(cboard));
  b->width = width;
  b->height = height;
  b->board = tehboard;
  return b;
}

point * make_point(int x, int y) {
  point *p = (point *)malloc(sizeof(point));
  p->x = x;
  p->y = y;
  return p;
}

point * real_native_execute_square(cboard* in,
				   point* pt_in,
				   cboard* out,
				   int hugs,
				   int beard,
				   int reversedX)
{
	struct point* result = (point*) malloc(sizeof(point));
	struct point pt;
	pt.x = pt_in->x; pt.y = pt_in->y;
	result->x = -1;
	char xy = CELL_BOARDPTR(in, pt.x, pt.y);
	int dx, dy;
	if(IS_ROCK_LIKE(xy))
		{
		  if(IS_EMPTY(CELL_BOARDPTR(in,pt.x,pt.y-1)))
				{
				  CELL_BOARDPTR(out,pt.x,pt.y) = ' ';
					CELL_BOARDPTR(out,pt.x,pt.y-1) = xy;
					result->x = pt.x;
					result->y = pt.y-1;
				}
			else if(IS_ROCK_LIKE(CELL_BOARDPTR(in,pt.x,pt.y-1))
							&& IS_EMPTY(CELL_BOARDPTR(in,pt.x+1,pt.y))
							&& IS_EMPTY(CELL_BOARDPTR(in,pt.x+1,pt.y-1)))
				{
					CELL_BOARDPTR(out,pt.x+1,pt.y-1) = xy;
					CELL_BOARDPTR(out,pt.x,pt.y) = ' ';
					result->x = pt.x + 1;
					result->y = pt.y - 1;
				}
			else if(IS_HUG(CELL_BOARDPTR(in,pt.x,(pt.y-1)))
							&& IS_EMPTY(CELL_BOARDPTR(in,pt.x+1,pt.y))
							&& IS_EMPTY(CELL_BOARDPTR(in,pt.x+1,pt.y-1)))
				{
					CELL_BOARDPTR(out,pt.x,pt.y) = ' ';
					CELL_BOARDPTR(out,pt.x+1,pt.y-1) = xy;
					//TODO: Append to list of moving rocks
				}
		}
	else if(IS_CLOSED_LIFT(xy) && hugs == 0)
		{
			CELL_BOARDPTR(out,pt.x,pt.y) = 'O';
		}
	else if(IS_BEARD(xy) && beard != 0)
		{
			for(dx = -1; dx <= 1; ++dx)
				for(dy = -1; dy <= 1; ++dy)
					{
						if(dx == pt.x && dy == pt.y) continue;

						if(IS_EMPTY(CELL_BOARDPTR(in,dx,dy)))
							CELL_BOARDPTR(out,dx,dy) = 'W';
					}
		}
	else if(IS_EMPTY(CELL_BOARDPTR(in,pt.x,pt.y)))
		{
			CELL_BOARDPTR(out,pt.x,pt.x) = xy;
		}
	return result;
}

point * native_execute_square(cboard * in,
			      point * pt_in,
			      cboard * out,
			      int hugs,
			      int beard) {
  return real_native_execute_square(in,
				    pt_in,
				    out,
				    hugs,
				    beard,
				    0);
}


/*
int simulate_board(struct cboard* b, int hugs, char beard)
{
	struct cboard newboard;
	newboard.board = malloc(b.width*b.height);

	if(newboard.board == 0) return -1;

	for(int x = 0; x < width; x++)
		{
			for(int y = 0; y < height; y++)
				{
					execute_square(
				}
		}
}
// */
