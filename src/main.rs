use colored::Colorize;
use rand::seq::SliceRandom;
use std::collections::HashMap;
use std::ops::Index;
use std::ops::IndexMut;
use std::{fs::File, io::Read};
fn main() {
    let mut file = File::open("chars.txt").unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();

    //adjacency rules
    let rules: HashMap<tile_type, HashMap<dir, Vec<tile_type>>> = HashMap::new();

    //input board (should only be characters)
    let in_board: Vec<Vec<char>> = contents.lines().map(|l| l.chars().collect()).collect();

    //the real board
    //let mut board = board::new(in_board.len() * in_board[0].len(), rules);
    let mut board = board::new((in_board.len() * 2, in_board[0].len() * 2), rules);

    //iterate over our input board row-major to generate rules
    for (row, line) in in_board.iter().enumerate() {
        //board.map.push(Vec::new());
        for (col, c) in line.iter().enumerate() {
            print!("{c}");

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

            //board.map[row].push(tile::fresh((row, col)));
        }
        print!("\n");
    }

    for (k, v) in &board.rules {
        println!("{:?}: {:?}", k, v);
    }

    let mut undo: Vec<board> = Vec::new();
    undo.push(board.clone());

    while board.remaining > 0 {
        //keep the board state before collapse in the undo stack

        match board.collapse() {
            Ok(_v) => {
                undo.push(board.clone());
            }
            //TODO: ig if we failed very very early this could panic??
            Err(_e) => board = undo.pop().unwrap(),
        }

        for row in &board.map {
            for c in row {
                print!("{}", c.rep);
            }
            print!("\n")
        }
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
    remaining: usize,
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

        Self {
            map: map,
            remaining: size.0 * size.1,
            rules,
        }
    }

    //collapse a tile's position down to a single position, and update all its neighbors positions ~~recursively~~
    fn collapse(&mut self) -> Result<String, String> {
        //chose the tile with the lowest entropy to collapse
        let chosen_i = self.chose_tile_to_collapse();
        println!(
            "{} {:?},{:?}",
            "chosing to collapse:".red(),
            chosen_i,
            self.map[chosen_i.0][chosen_i.1]
        );

        //concatate all legal states based on neighbor rules
        let mut new_pos: Vec<tile_type> = Vec::new();

        let neighbors = self.get_neighbors(chosen_i);

        //look at each of our tile's neighbors
        for neighbor in neighbors {
            let superposition = &self[neighbor.tile].position;
            for position in superposition {
                for allowed in &self.rules[position][&neighbor.anti_direction] {
                    if !new_pos.contains(&allowed) {
                        new_pos.push(*allowed);
                    }
                }
            }
        }

        //now just chose one of the allowable positions at random
        let choice = new_pos.choose(&mut rand::thread_rng());
        if choice.is_none() {
            return Err("could not collapse tile".to_string());
        }
        self.map[chosen_i.0][chosen_i.1].t = Some(*choice.unwrap());
        //since we can only have a single position now, just give ourselves an empty superposition vector for comparision
        self.map[chosen_i.0][chosen_i.1].position = Vec::new();
        self.map[chosen_i.0][chosen_i.1].rep =
            tile_type_to_char(self.map[chosen_i.0][chosen_i.1].t.unwrap());

        println!(
            "{} {:?} \nfrom {:?}",
            "chose:".red(),
            self.map[chosen_i.0][chosen_i.1].t.unwrap(),
            new_pos
        );

        self.update(chosen_i);

        self.remaining -= 1;

        return Ok("collapsed tile succcessfully".to_string());
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
            .and_then(|f| Some(f[pos.1].coords));

        //println!("north: {:?}", n.north);

        //south
        n.south = pos
            .0
            .checked_add(1)
            .and_then(|e| self.map.get(e))
            .and_then(|f| Some(f[pos.1].coords));

        //println!("south: {:?}", n.south);

        //west
        n.west = pos
            .1
            .checked_sub(1)
            .and_then(|e| self.map[pos.0].get(e))
            .and_then(|f| Some(f.coords));

        //println!("west: {:?}", n.west);

        //east
        n.east = pos
            .1
            .checked_add(1)
            .and_then(|e| self.map[pos.0].get(e))
            .and_then(|f| Some(f.coords));

        //println!("east: {:?}", n.east);

        //println!("neighbors: {:?}", n);

        return n;
    }

    //takes 1 tile, which we can assume to have a concrete type, and udpates its neighbors positions
    fn update(&mut self, center_tile: (usize, usize)) {
        println!(
            "{}{:?}",
            "updating neighbors of: ".green(),
            self[center_tile]
        );

        let neighbors = self.get_neighbors(center_tile);
        let concrete_type = self[center_tile].t.unwrap();

        for neighbor in neighbors {
            if !self[neighbor.tile].t.is_some() {
                println!("\n{}{:?}", "updating: ".green(), neighbor.tile);
                println!(
                    "{} {:?}",
                    "initial position".green(),
                    self[neighbor.tile].position
                );
                let mut new_position = self[neighbor.tile].position.clone();
                new_position
                    .retain(|e| self.rules[&concrete_type][&neighbor.direction].contains(e));
                self[neighbor.tile].position = new_position;

                if self[neighbor.tile].position.len() == 1 {
                    self[neighbor.tile].t = Some(self[neighbor.tile].position[0]);
                    self[neighbor.tile].rep = tile_type_to_char(self[neighbor.tile].t.unwrap());
                    self.remaining -= 1;
                }

                println!(
                    "{} {:?}",
                    "final position".green(),
                    self[neighbor.tile].position
                );
            }
        }
    }
}

#[allow(non_camel_case_types)]
#[derive(Debug)]
struct neighbors {
    north: Option<(usize, usize)>,
    south: Option<(usize, usize)>,
    east: Option<(usize, usize)>,
    west: Option<(usize, usize)>,
}

#[allow(non_camel_case_types)]
struct neighborIterElement {
    direction: dir,
    anti_direction: dir,
    tile: (usize, usize),
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
