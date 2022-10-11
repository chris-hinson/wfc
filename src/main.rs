//use colored::Colorize;
use rand::seq::SliceRandom;
use std::collections::HashMap;
use std::env;
use std::ops::Index;
use std::ops::IndexMut;
use std::{fs::File, io::Read};

use rand::thread_rng;
fn main() {
    let args: Vec<String> = env::args().collect();

    println!("{args:?}");

    //let mut file = File::open("chars.txt").unwrap();
    let mut file = File::open(args[3].clone()).unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();

    //adjacency rules
    let rules: HashMap<tile_type, HashMap<dir, Vec<tile_type>>> = HashMap::new();

    //input board (should only be characters)
    let in_board: Vec<Vec<char>> = contents.lines().map(|l| l.chars().collect()).collect();

    //the real board
    //let mut board = board::new((100, 100), rules);
    let mut board = board::new(
        (
            args[1].parse::<usize>().unwrap(),
            args[2].parse::<usize>().unwrap(),
        ),
        rules,
    );

    //iterate over our input board row-major to generate rules
    for (row, line) in in_board.iter().enumerate() {
        for (col, c) in line.iter().enumerate() {
            //print!("{c}");

            //get the rules entry for this kind of tile
            //if it doesnt exist, add a tile_type vec for each direction
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
            row.checked_sub(1)
                .and_then(|r| in_board.get(r))
                .and_then(|c| c.get(col))
                .and_then(|e| {
                    let north_type = char_to_tile_type(*e);
                    cur.entry(dir::NORTH).and_modify(|allowed| {
                        if !allowed.contains(&north_type) {
                            allowed.push(north_type);
                        }
                    });

                    Some(true)
                });

            //SOUTH
            row.checked_add(1)
                .and_then(|r| in_board.get(r))
                .and_then(|c| c.get(col))
                .and_then(|e| {
                    let north_type = char_to_tile_type(*e);
                    cur.entry(dir::SOUTH).and_modify(|allowed| {
                        if !allowed.contains(&north_type) {
                            allowed.push(north_type);
                        }
                    });

                    Some(true)
                });

            //WEST

            col.checked_sub(1)
                .and_then(|col| in_board[row].get(col))
                .and_then(|char| {
                    let north_type = char_to_tile_type(*char);
                    cur.entry(dir::WEST).and_modify(|allowed| {
                        if !allowed.contains(&north_type) {
                            allowed.push(north_type);
                        }
                    });

                    Some(true)
                });

            //EAST

            col.checked_add(1)
                .and_then(|col| in_board[row].get(col))
                .and_then(|char| {
                    let north_type = char_to_tile_type(*char);
                    cur.entry(dir::EAST).and_modify(|allowed| {
                        if !allowed.contains(&north_type) {
                            allowed.push(north_type);
                        }
                    });

                    Some(true)
                });
        }
        //print!("\n");
    }

    /*for (k, v) in &board.rules {
        println!("{:?}: {:?}", k, v);
    }*/

    let solvable = board.collapse(board.chose_tile_to_collapse());
    println!("solved? {solvable:?}");

    println!("\n");
    for row in &board.map {
        for c in row {
            print!("{}", c.rep);
        }
        print!("\n")
    }

    //println!("{:?}", board.map);
}

//TODO: reconsider internalizing the map within this struct.
//because it is a local variable, we cant pass around references to tiles within it without the borrow checker getting mad
//so we instead have to refer to tiles by coords: (usize,usize)
#[allow(non_camel_case_types)]
#[derive(Debug, Clone)]
struct board {
    map: Vec<Vec<tile>>,
    rules: HashMap<tile_type, HashMap<dir, Vec<tile_type>>>,
}

impl Index<(usize, usize)> for board {
    type Output = tile;

    fn index(&self, index: (usize, usize)) -> &Self::Output {
        &self.map[index.0][index.1]
    }
}

impl IndexMut<(usize, usize)> for board {
    fn index_mut(&mut self, index: (usize, usize)) -> &mut Self::Output {
        &mut self.map[index.0][index.1]
    }
}

impl board {
    fn new(size: (usize, usize), rules: HashMap<tile_type, HashMap<dir, Vec<tile_type>>>) -> Self {
        let mut map: Vec<Vec<tile>> = Vec::new();

        for row in 0..size.0 {
            map.push(Vec::new());
            for col in 0..size.1 {
                map[row].push(tile::fresh((row, col)));
            }
        }

        Self { map, rules }
    }

    //TODO: this has a fucking abysmal runtime, please figure out a way to make it better
    //this will make sure no tiles on the board are breaking adjacency rules. it will NOT check if we have a completed board
    fn valid_position(&self) -> bool {
        for row in &self.map {
            for col in row {
                //empty superpositions are not valid unless the tile has a concrete type
                if col.position.len() == 0 && col.t.is_none() {
                    return false;
                }

                //only way we could be breaking adjacency rules is if this tile has a concrete position and one of its neighbors
                //ALSO has a conrete position, which is not allowed beside it
                if col.t.is_some() {
                    for n in self.get_neighbors(col.coords) {
                        if n.tile.t.is_some() {
                            if !self.rules[&n.tile.t.unwrap()][&n.anti_direction]
                                .contains(&col.t.unwrap())
                            {
                                return false;
                            }
                        }
                    }
                }
            }
        }

        return true;
    }

    fn is_solved(&self) -> bool {
        if !self.valid_position() {
            return false;
        }

        return !self.map.iter().flatten().any(|t| t.t == None);
    }

    //chose the tile on the board with the lowest entropy and return its coords within the map
    fn chose_tile_to_collapse(&self) -> (usize, usize) {
        return self
            .map
            .iter()
            .flatten()
            .min_by(|x, y| x.entropy().cmp(&y.entropy()))
            .unwrap()
            .coords;
    }

    //returns a neighbors struct, with inidces into the map of the neighboring tiles
    fn get_neighbors(&self, pos: (usize, usize)) -> neighbors {
        let mut n = neighbors::new();

        //north
        n.north = pos
            .0
            .checked_sub(1)
            .and_then(|e| self.map.get(e))
            .and_then(|f| Some(f[pos.1].clone()));

        //println!("north: {:?}", n.north);

        //south
        n.south = pos
            .0
            .checked_add(1)
            .and_then(|e| self.map.get(e))
            .and_then(|f| Some(f[pos.1].clone()));

        //println!("south: {:?}", n.south);

        //west
        n.west = pos
            .1
            .checked_sub(1)
            .and_then(|e| self.map[pos.0].get(e))
            .and_then(|f| Some(f.clone()));

        //println!("west: {:?}", n.west);

        //east
        n.east = pos
            .1
            .checked_add(1)
            .and_then(|e| self.map[pos.0].get(e))
            .and_then(|f| Some(f.clone()));

        //println!("east: {:?}", n.east);

        //println!("neighbors: {:?}", n);

        return n;
    }

    //takes 1 tile, collapses its state down to a concrete type, and udpates its neighbors super-positions
    //returns a result of a tile vec, either to put it in the backup queue or immeadiately undo it
    //fn collapse(&mut self, center_tile: (usize, usize)) -> Result<Vec<tile>, Vec<tile>> {
    fn collapse(&mut self, center_tile: (usize, usize)) -> bool {
        //gtfo of here and return our way up the call stack if we have solved our board
        if self.is_solved() {
            return true;
        }

        //backup the superposition and zero it out
        let mut random_pos = self[center_tile].position.clone();
        let backup_pos = self[center_tile].position.clone();
        self[center_tile].position = Vec::new();

        //iterate through the possible positions of the superposition
        random_pos.shuffle(&mut thread_rng());
        for pos in random_pos {
            //tentatively give our tile this concrete position and give it a char rep
            self[center_tile].t = Some(pos);
            self[center_tile].rep = tile_type_to_char(pos);

            //backup neighbors and
            // update neighbors superpositions according to the subposition we are trying
            let old_neighbors = self.get_neighbors(center_tile);
            for mut n in self.get_neighbors(center_tile) {
                n.tile
                    .position
                    .retain(|t| self.rules[&pos][&n.direction].contains(t));
            }

            //if this subposition is a valid position, call solve on the next tile to be collapse
            if self.valid_position() {
                // if we are not in a solved board, continue recursing, otherwise, return our way up the call stack
                if self.collapse(self.chose_tile_to_collapse()) {
                    return true;
                } else {
                }
            } else {
                for n in old_neighbors {
                    self[n.tile.coords] = n.tile.clone();
                }
            }
        }
        self[center_tile].t = None;
        self[center_tile].position = backup_pos.clone();
        return false;
    }
}

/*#[allow(non_camel_case_types)]
#[derive(Debug)]
struct neighbors {
    north: Option<(usize, usize)>,
    south: Option<(usize, usize)>,
    east: Option<(usize, usize)>,
    west: Option<(usize, usize)>,
}*/
#[allow(non_camel_case_types)]
#[derive(Debug)]
struct neighbors {
    north: Option<tile>,
    south: Option<tile>,
    east: Option<tile>,
    west: Option<tile>,
}

#[allow(non_camel_case_types)]
struct neighborIterElement {
    direction: dir,
    anti_direction: dir,
    tile: tile,
}

impl IntoIterator for neighbors {
    type Item = neighborIterElement;
    type IntoIter = std::vec::IntoIter<neighborIterElement>;

    fn into_iter(self) -> Self::IntoIter {
        let mut neighbors: Vec<neighborIterElement> = Vec::new();

        self.north.and_then(|f| {
            Some(neighbors.push(neighborIterElement {
                direction: dir::NORTH,
                anti_direction: dir::SOUTH,
                tile: f,
            }))
        });

        self.south.and_then(|f| {
            Some(neighbors.push(neighborIterElement {
                direction: dir::SOUTH,
                anti_direction: dir::NORTH,
                tile: f,
            }))
        });

        self.east.and_then(|f| {
            Some(neighbors.push(neighborIterElement {
                direction: dir::EAST,
                anti_direction: dir::WEST,
                tile: f,
            }))
        });

        self.west.and_then(|f| {
            Some(neighbors.push(neighborIterElement {
                direction: dir::WEST,
                anti_direction: dir::EAST,
                tile: f,
            }))
        });

        return neighbors.into_iter();
    }
}

impl neighbors {
    fn new() -> Self {
        Self {
            north: None,
            south: None,
            east: None,
            west: None,
        }
    }
}

#[derive(PartialEq, Hash, Eq, Debug, Clone)]
#[allow(non_camel_case_types)]
enum dir {
    WEST,
    NORTH,
    EAST,
    SOUTH,
}

impl IntoIterator for dir {
    type Item = dir;
    type IntoIter = std::vec::IntoIter<dir>;

    fn into_iter(self) -> Self::IntoIter {
        return vec![dir::NORTH, dir::SOUTH, dir::EAST, dir::WEST].into_iter();
    }
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
        if self.t.is_some() {
            return usize::MAX;
        } else {
            return self.position.len();
        }
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

fn tile_type_to_char(t: tile_type) -> char {
    return match t {
        tile_type::HORIZ_BAR => '━',
        tile_type::VERT_BAR => '┃',
        tile_type::DOWN_RIGHT => '┏',
        tile_type::DOWN_LEFT => '┓',
        tile_type::UP_LEFT => '┛',
        tile_type::UP_RIGHT => '┗',
        tile_type::VERT_RIGHT => '┣',
        tile_type::VERT_LEFT => '┫',
        tile_type::HORIZ_DOWN => '┳',
        tile_type::HORIZ_UP => '┻',
        tile_type::CROSS => '╋',
        tile_type::DOWN_LEFT_ROUND => '╮',
        tile_type::DOWN_RIGHT_ROUND => '╭',
        tile_type::UP_LEFT_ROUND => '╯',
        tile_type::UP_RIGHT_ROUND => '╰',
    };
}
