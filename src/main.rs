use rand::seq::SliceRandom;
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
    let mut board = board::new(in_board.len() * in_board[0].len(), rules);
    //vec![vec![tile::fresh(rep)]; in_board.len()];

    //iterate over our input board row-major
    for (row, line) in in_board.iter().enumerate() {
        board.map.push(Vec::new());
        for (col, c) in line.iter().enumerate() {
            print!("{c}");

            //get the rules entry for this kind of tile
            //if it doesnt exist, add a tile vec for each direction
            let cur = board
                .rules
                .entry(char_to_tile_type(*c))
                .or_insert(HashMap::from([
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

            board.map[row].push(tile::fresh((row, col)));
        }
        print!("\n");
    }

    for (k, v) in &board.rules {
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
        board.collapse();

        //update its neighbors posibilities recursively
    }
}

#[allow(non_camel_case_types)]
struct board {
    map: Vec<Vec<tile>>,
    remaining: usize,
    rules: HashMap<tile_type, HashMap<dir, Vec<tile_type>>>,
}
impl<'a> board {
    fn new(size: usize, rules: HashMap<tile_type, HashMap<dir, Vec<tile_type>>>) -> Self {
        Self {
            map: Vec::new(),
            remaining: size,
            rules,
        }
    }

    //one iteration of the collapse algorithm
    fn collapse(&mut self) -> Vec<tile_type> {
        //chose the tile with the lowest entropy to collapse
        let chosen_i = self.chose_tile_to_collapse();
        //let chosen = &mut self.map[chosen_i.0][chosen_i.1];
        //concatate all legal states based on neighbor rules
        let mut new_pos: Vec<tile_type> = Vec::new();

        //TODO: implement an iterator for me to clean up the code below it
        let mut neighbors = self.get_neighbors(chosen_i);

        //north
        if neighbors.north.is_some() {
            for possibility in &neighbors.north.unwrap().position {
                for t in &self.rules.get(possibility).unwrap()[&dir::SOUTH] {
                    if !new_pos.contains(&t) {
                        new_pos.push(t.clone());
                    }
                }
            }
        }
        //east
        if neighbors.east.is_some() {
            for possibility in &neighbors.east.unwrap().position {
                for t in &mut self.rules[possibility][&dir::WEST] {
                    if !new_pos.contains(t) {
                        new_pos.push(t.clone());
                    }
                }
            }
        }
        //south
        if neighbors.south.is_some() {
            for possibility in &neighbors.south.unwrap().position {
                for t in &mut self.rules[possibility][&dir::NORTH] {
                    if !new_pos.contains(t) {
                        new_pos.push(t.clone());
                    }
                }
            }
        }
        //west
        if neighbors.west.is_some() {
            for possibility in &neighbors.west.unwrap().position {
                for t in &mut self.rules[possibility][&dir::EAST] {
                    if !new_pos.contains(t) {
                        new_pos.push(t.clone());
                    }
                }
            }
        }

        //now just chose one of the allowable positions at random
        self.map[chosen_i.0][chosen_i.1].t =
            Some(*new_pos.choose(&mut rand::thread_rng()).unwrap());

        //need to update neighbors

        return new_pos;
    }

    //chose the tile on the board with the lowest entropy and return its coords within the map
    fn chose_tile_to_collapse(&self) -> (usize, usize) {
        return self
            .map
            .iter()
            .flatten()
            .min_by(|x, y| x.position.len().cmp(&y.position.len()))
            .unwrap()
            .coords;
    }

    //returns a neighbors struct, with references to the four neighboring tiles
    fn get_neighbors(&'a mut self, pos: (usize, usize)) -> neighbors<'a> {
        let mut n = neighbors::new();

        //let mut n = Vec::new();

        //let t = &self.map[pos.0][pos.1];

        //north
        if self.map.get(((pos.0 as i32) - 1) as usize).is_some() {
            //n.push(&self.map[pos.0 - 1][pos.1]);
            n.north = Some(&mut self.map[pos.0 - 1][pos.1]);
        }
        //south
        if self.map.get((pos.0 + 1) as usize).is_some() {
            n.south = Some(&mut self.map[pos.0 + 1][pos.1]);
        }
        //west
        if self.map[pos.0].get(((pos.1 as i32) - 1) as usize).is_some() {
            n.west = Some(&mut self.map[pos.0][pos.1 - 1]);
        }
        //east
        if self.map[pos.0].get(((pos.1 as i32) + 1) as usize).is_some() {
            n.east = Some(&mut self.map[pos.0][pos.1 + 1]);
        }

        return n;
    }

    fn update(&mut self) {}
}

struct neighbors<'a> {
    north: Option<&'a mut tile>,
    south: Option<&'a mut tile>,
    east: Option<&'a mut tile>,
    west: Option<&'a mut tile>,
}

impl<'a> neighbors<'a> {
    fn new() -> Self {
        Self {
            north: None,
            south: None,
            east: None,
            west: None,
        }
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
    //tile only has a type once it has been fully collapsed
    t: Option<tile_type>,
    position: Vec<tile_type>,
}
impl tile {
    fn fresh(coords: (usize, usize)) -> Self {
        Self {
            coords,
            rep: 'X',
            t: None,
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
