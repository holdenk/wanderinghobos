#include "csimulate.h"

cboard * make_board(int width, int height, char * tehboard) {
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


extern point* native_execute_square(cboard* in,
				   point* pt_in,
				   cboard* out,
				   int hugs,
				   int beard)
{
	struct point* result = (point*) malloc(sizeof(point));
	struct point pt;
	pt.x = pt_in->x; pt.y = pt_in->y;
	result->x = -1;
	char xy = CELL_BOARDPTR(in, pt.x, pt.y);
	int dx, dy;
	if(IS_ROCK_LIKE(xy)) {
      if (IS_EMPTY(CELL_BOARDPTR(in,pt.x,pt.y-1))) {
					CELL_BOARDPTR(out,pt.x,pt.y) = ' ';
					CELL_BOARDPTR(out,pt.x,pt.y-1) = xy;
					result->x = pt.x;
					result->y = pt.y-1;
			} else if (IS_ROCK_LIKE(CELL_BOARDPTR(in,pt.x,pt.y-1))
							&& IS_EMPTY(CELL_BOARDPTR(in,pt.x+1,pt.y))
							&& IS_EMPTY(CELL_BOARDPTR(in,pt.x+1,pt.y-1))) {
					CELL_BOARDPTR(out,pt.x,pt.y) = ' ';
					CELL_BOARDPTR(out,pt.x+1,pt.y-1) = xy;
					result->x = pt.x + 1;
					result->y = pt.y - 1;
			} else if (IS_ROCK_LIKE(CELL_BOARDPTR(in,pt.x,pt.y-1))
							&& ((((pt.x+1 < in->width) && (pt.y < in->height)) && !IS_EMPTY(CELL_BOARDPTR(in,pt.x+1,pt.y)))
							    || (((pt.x+1 < in->width) && (pt.y < in->height)) && !IS_EMPTY(CELL_BOARDPTR(in,pt.x+1,pt.y-1))))
							&& (IS_EMPTY(CELL_BOARDPTR(in, pt.x-1, pt.y)))
							&& (IS_EMPTY(CELL_BOARDPTR(in, pt.x-1, pt.y-1)))) {
					CELL_BOARDPTR(out,pt.x,pt.y) = ' ';
					CELL_BOARDPTR(out,pt.x-1,pt.y-1) = xy;
					result->x = pt.x - 1;
					result->y = pt.y - 1;
			} else if (IS_HUG(CELL_BOARDPTR(in,pt.x,(pt.y-1)))
							&& IS_EMPTY(CELL_BOARDPTR(in,pt.x+1,pt.y))
							&& IS_EMPTY(CELL_BOARDPTR(in,pt.x+1,pt.y-1))) {
					CELL_BOARDPTR(out,pt.x,pt.y) = ' ';
					CELL_BOARDPTR(out,pt.x+1,pt.y-1) = xy;
					result->x = pt.x + 1;
					result->y = pt.y - 1;
					//TODO: Append to list of moving rocks
			} else {
					CELL_BOARDPTR(out,pt.x,pt.y) = xy;
      }
	} else if (IS_CLOSED_LIFT(xy) && hugs == 0) {
			CELL_BOARDPTR(out,pt.x,pt.y) = 'O';
	} else if (IS_BEARD(xy) && beard != 0) {
			for(dx = -1; dx <= 1; ++dx)
				for(dy = -1; dy <= 1; ++dy) {
						if(dx == pt.x && dy == pt.y) continue;

						if(IS_EMPTY(CELL_BOARDPTR(in, pt.x+dx, pt.y+dy)))
							CELL_BOARDPTR(out, pt.x+dx, pt.y+dy) = 'W';
				}
	} else if (IS_EMPTY(CELL_BOARDPTR(in,pt.x,pt.y))) {
      CELL_BOARDPTR(out,pt.x,pt.x) = xy;
	}
	return result;
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
