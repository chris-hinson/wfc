use std::collections::HashMap;
use std::{fs::File, io::Read};
fn main() {
    let mut file = File::open("chars.txt").unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();

    //println!("{:?}", contents);

    /*let chars: [char; 15] = [
        '━', '┃', '┏', '┓', '┛', '┗', '┣', '┫', '┳', '┻', '╋', '╮', '╭', '╯', '╰',
    ];*/

    let mut rules: HashMap<tile_type, HashMap<dir, Vec<tile_type>>> = HashMap::new();
    let in_board: Vec<Vec<char>> = contents.lines().map(|l| l.chars().collect()).collect();
    //let mut map: Vec<Vec<tile>> = Vec::new();
    let mut board = board::new(in_board.len() * in_board[0].len());
    //vec![vec![tile::fresh(rep)]; in_board.len()];

    //iterate over our input board row-major
    for (row, line) in in_board.iter().enumerate() {
        board.map.push(Vec::new());
        for (col, c) in line.iter().enumerate() {
            print!("{c}");

            //get the rules entry for this kind of tile
            //if it doesnt exist, add a tile vec for each direction
            let cur = rules.entry(char_to_tile_type(*c)).or_insert(HashMap::from([
                (dir::WEST, Vec::new()),
                (dir::NORTH, Vec::new()),
                (dir::EAST, Vec::new()),
                (dir::SOUTH, Vec::new()),
            ]));

            //north
            //funky casting to allow us to deal with row 0
            if in_board.get((row as i32 - 1) as usize).is_some() {
                //modify the hashmap entry for the NORTH direction
                cur.entry(dir::NORTH).and_modify(|vec| {
                    let t = char_to_tile_type(in_board[row - 1][col]);
                    //only add this to the rules if it is not already in the rules
                    if !vec.contains(&t) {
                        vec.push(t);
                    }
                });
            }

            //SOUTH
            if in_board.get(row + 1).is_some() {
                cur.entry(dir::SOUTH).and_modify(|vec| {
                    let t = char_to_tile_type(in_board[row + 1][col]);
                    if !vec.contains(&t) {
                        vec.push(t);
                    }
                });
            }

            //WEST
            if in_board[row].get(((col as i32) - 1) as usize).is_some() {
                cur.entry(dir::WEST).and_modify(|vec| {
                    let t = char_to_tile_type(in_board[row][col - 1]);
                    if !vec.contains(&t) {
                        vec.push(t);
                    }
                });
            }

            //EAST
            if in_board[row].get(col + 1).is_some() {
                cur.entry(dir::EAST).and_modify(|vec| {
                    let t = char_to_tile_type(in_board[row][col + 1]);
                    if !vec.contains(&t) {
                        vec.push(t);
                    }
                });
            }

            board.map[row].push(tile::fresh(*c, (row, col)));
        }
        print!("\n");
    }

    for (k, v) in rules {
        println!("{:?}: {:?}", k, v);
    }

    /*while map
    .iter()
    .flatten()
    .filter(|e| e.position.len() > 0)
    .collect::<Vec<&tile>>()
    .len()
    > 0*/
    while board.remaining > 0 {
        //chose a tile to collapse
        //let chosen = board.chose_tile_to_collapse();

        //collapse it

        //state = concatenate all neighbors allowed positions, and dedup, then chose at random
        let new_pos = board.collapse();

        //update its neighbors posibilities recursively
    }
}

#[allow(non_camel_case_types)]
struct board {
    map: Vec<Vec<tile>>,
    remaining: usize,
}
impl board {
    fn new(size: usize) -> Self {
        Self {
            map: Vec::new(),
            remaining: size,
        }
    }

    //chose the tile on the board with the lowest entropy and return a mutuable ref to it
    fn chose_tile_to_collapse(&self) -> (usize, usize) {
        return self
            .map
            .iter()
            .flatten()
            .min_by(|x, y| x.position.len().cmp(&y.position.len()))
            .unwrap()
            .coords;
    }

    //
    fn collapse(&mut self) -> Vec<tile_type> {
        //chose the tile with the lowest entropy to collapse
        let chosen = self.chose_tile_to_collapse();

        //concatate all neighbors legal states
        //TODO: need to only consider some of neighbors legalities
        let mut pos: Vec<tile_type> = Vec::new();
        for n in self.get_neighbors(&self.map[chosen.0][chosen.1]) {
            //TODO: why the fuck does vec.apped require the vec being appended to be mutable??
            for e in &n.position {
                if !pos.contains(&e) {
                    //note: this clone is fine bc enums are tiny
                    pos.push(e.clone());
                }
            }
        }

        return pos;
    }

    fn get_neighbors(&self, t: &tile) -> Vec<&tile> {
        let mut n = Vec::new();

        //north
        if self.map.get(((t.coords.0 as i32) - 1) as usize).is_some() {
            n.push(&self.map[t.coords.0 - 1][t.coords.1]);
        }
        //south
        if self.map.get((t.coords.0 + 1) as usize).is_some() {
            n.push(&self.map[t.coords.0 + 1][t.coords.1]);
        }
        //west
        if self.map[t.coords.0]
            .get(((t.coords.1 as i32) - 1) as usize)
            .is_some()
        {
            n.push(&self.map[t.coords.0][t.coords.1 - 1]);
        }
        //east
        if self.map[t.coords.0]
            .get(((t.coords.1 as i32) + 1) as usize)
            .is_some()
        {
            n.push(&self.map[t.coords.0][t.coords.1 + 1]);
        }

        return n;
    }
}

#[derive(PartialEq, Hash, Eq, Debug)]
#[allow(non_camel_case_types)]
enum dir {
    WEST,
    NORTH,
    EAST,
    SOUTH,
}

//lets keep track of our tile types, along with a struct of their adjacency rules
#[derive(PartialEq, Hash, Eq, Debug, Clone, Copy)]
#[allow(non_camel_case_types)]
enum tile_type {
    HORIZ_BAR,
    VERT_BAR,
    DOWN_RIGHT,
    DOWN_LEFT,
    UP_LEFT,
    UP_RIGHT,
    VERT_RIGHT,
    VERT_LEFT,
    HORIZ_DOWN,
    HORIZ_UP,
    CROSS,
    DOWN_LEFT_ROUND,
    DOWN_RIGHT_ROUND,
    UP_LEFT_ROUND,
    UP_RIGHT_ROUND,
}

//this struct defines the rules for a tile type
#[derive(PartialEq, Hash, Eq, Debug, Clone)]
#[allow(non_camel_case_types)]
struct tile {
    coords: (usize, usize),
    rep: char,
    t: tile_type,
    position: Vec<tile_type>,
}
impl tile {
    fn fresh(rep: char, coords: (usize, usize)) -> Self {
        Self {
            coords,
            rep,
            t: char_to_tile_type(rep),
            position: vec![
                tile_type::HORIZ_BAR,
                tile_type::VERT_BAR,
                tile_type::DOWN_RIGHT,
                tile_type::DOWN_LEFT,
                tile_type::UP_LEFT,
                tile_type::UP_RIGHT,
                tile_type::VERT_RIGHT,
                tile_type::VERT_LEFT,
                tile_type::HORIZ_DOWN,
                tile_type::HORIZ_UP,
                tile_type::CROSS,
                tile_type::DOWN_LEFT_ROUND,
                tile_type::DOWN_RIGHT_ROUND,
                tile_type::UP_LEFT_ROUND,
                tile_type::UP_RIGHT_ROUND,
            ],
        }
    }

    fn entropy(&self) -> usize {
        return self.position.len();
    }
}

//takes a unicode char, and out
fn char_to_tile_type(in_char: char) -> tile_type {
    return match in_char {
        '━' => tile_type::HORIZ_BAR,
        '┃' => tile_type::VERT_BAR,
        '┏' => tile_type::DOWN_RIGHT,
        '┓' => tile_type::DOWN_LEFT,
        '┛' => tile_type::UP_LEFT,
        '┗' => tile_type::UP_RIGHT,
        '┣' => tile_type::VERT_RIGHT,
        '┫' => tile_type::VERT_LEFT,
        '┳' => tile_type::HORIZ_DOWN,
        '┻' => tile_type::HORIZ_UP,
        '╋' => tile_type::CROSS,
        '╮' => tile_type::DOWN_LEFT_ROUND,
        '╭' => tile_type::DOWN_RIGHT_ROUND,
        '╯' => tile_type::UP_LEFT_ROUND,
        '╰' => tile_type::UP_RIGHT_ROUND,
        _ => panic!("BAD INPUT CHAR"),
    };
}
