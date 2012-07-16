struct board
{
	int width, height;
	char* board;
};

struct point
{
	int x, y;
};

#define APT(x,y,b) y*w+x
#define CELL_BOARDPTR(b,x,y) b->board[APT(x,y,b->width)]
#define IS_ROCK_LIKE(a) (a == '*' || a == '@')
#define IS_EMPTY(a) (a == ' ')
#define IS_HUG(a) (a == '\\')
#define IS_CLOSED_LIFT(a) (a == 'L')
#define IS_BEARD(a) (a == 'W')

inline struct point execute_square(struct board* in,
																	 struct point& pt,
																	 struct board* out,
																	 int hugs,
																	 char beard)
{
	struct point result;
	char xy = CELL_BOARDPTR(in,pt.x,pt.y);
	if(IS_ROCK_LIKE(xy))
		{
			if(IS_EMPTY(CELL_BOARDPTR(in,pt.x,pt.y-1)))
				{
					CELL_BOARDPTR(out,pt.x,pt.y) = ' ';
					CELL_BOARDPTR(out,pt.x,pt.y-1) = xy;
					result.x = x;
					result.y = y-1;
				}
			else if(IS_ROCK_LIKE(CELL_BOARDPTR(in,pt.x,pt.y-1))
							&& IS_EMPTY(CELL_BOARDPTR(in,pt.x+1,pt.y))
							&& IS_EMPTY(CELL_BOARDPTR(in,pt.x+1,pt.y-1)))
				{
					CELL_BOARDPTR(out,pt.x+1,pt.y-1) = xy;
					CELL_BOARDPTR(out,pt.x,pt.y) = ' ';
					result.x = x + 1;
					result.y = y - 1;
				}
			else if(IS_HUG(CELL_BOARDPTR(in,pt.x,pt.y-1,))
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
			for(int dx = -1; dx <= 1; ++dx)
				for(int dy = -1; dy <= 1; ++dy)
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
}

int simulate_board(struct board* b, int hugs, char beard)
{
	struct board newboard;
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
