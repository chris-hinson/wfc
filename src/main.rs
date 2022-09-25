use rand::seq::SliceRandom;
use std::collections::HashMap;
use std::collections::VecDeque;
use std::{fs::File, io::Read};
fn main() {
    let mut file = File::open("chars.txt").unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();

    //println!("{:?}", contents);

    /*let chars: [char; 15] = [
        '━', '┃', '┏', '┓', '┛', '┗', '┣', '┫', '┳', '┻', '╋', '╮', '╭', '╯', '╰',
    ];*/

    let rules: HashMap<tile_type, HashMap<dir, Vec<tile_type>>> = HashMap::new();
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

    while board.remaining > 0 {
        println!("collapsing");
        board.collapse();
        for row in &board.map {
            for c in row {
                print!("{}", c.rep);
            }
            print!("\n")
        }
    }

    /*for row in board.map {
        for c in row {
            print!("{}", c.rep);
        }
        println!("\n")
    }*/
    println!("{:?}", board.map);
}

//TODO: reconsider internalizing the map within this struct.
//because it is a local variable, we cant pass around references to tiles within it without the borrow checker getting mad
//so we instead have to refer to tiles by coords: (usize,usize)
#[allow(non_camel_case_types)]
struct board {
    map: Vec<Vec<tile>>,
    remaining: usize,
    rules: HashMap<tile_type, HashMap<dir, Vec<tile_type>>>,
}
impl board {
    fn new(size: usize, rules: HashMap<tile_type, HashMap<dir, Vec<tile_type>>>) -> Self {
        Self {
            map: Vec::new(),
            remaining: size,
            rules,
        }
    }

    //collapse a tile's position down to a single position, and update all positions recursively
    fn collapse(&mut self) {
        //chose the tile with the lowest entropy to collapse
        let chosen_i = self.chose_tile_to_collapse();
        println!(
            "chose to collapse: {:?},{:?}",
            chosen_i, self.map[chosen_i.0][chosen_i.1]
        );

        //concatate all legal states based on neighbor rules
        let mut new_pos: Vec<tile_type> = Vec::new();

        //queue of tiles that need to be updated
        let mut update_me: VecDeque<(usize, usize)> = VecDeque::new();

        //TODO: implement an iterator for me to clean up the code below it
        let neighbors = self.get_neighbors(chosen_i);

        //north
        if neighbors.north.is_some() {
            update_me.push_back(neighbors.north.unwrap());
            let t = &self.map[neighbors.north.unwrap().0][neighbors.north.unwrap().1];
            for possibility in &t.position {
                //for t in &self.rules.get(possibility).unwrap()[&dir::SOUTH] {
                for t in &self.rules[possibility][&dir::SOUTH] {
                    if !new_pos.contains(&t) {
                        new_pos.push(t.clone());
                    }
                }
            }
        }
        //east
        if neighbors.east.is_some() {
            update_me.push_back(neighbors.east.unwrap());
            let t = &self.map[neighbors.east.unwrap().0][neighbors.east.unwrap().1];
            for possibility in &t.position {
                for t in &self.rules[possibility][&dir::WEST] {
                    if !new_pos.contains(t) {
                        new_pos.push(t.clone());
                    }
                }
            }
        }
        //south
        if neighbors.south.is_some() {
            update_me.push_back(neighbors.south.unwrap());
            let t = &self.map[neighbors.south.unwrap().0][neighbors.south.unwrap().1];
            for possibility in &t.position {
                for t in &self.rules[possibility][&dir::NORTH] {
                    if !new_pos.contains(t) {
                        new_pos.push(t.clone());
                    }
                }
            }
        }
        //west
        if neighbors.west.is_some() {
            update_me.push_back(neighbors.west.unwrap());
            let t = &self.map[neighbors.west.unwrap().0][neighbors.west.unwrap().1];
            for possibility in &t.position {
                for t in &self.rules[possibility][&dir::EAST] {
                    if !new_pos.contains(t) {
                        new_pos.push(t.clone());
                    }
                }
            }
        }

        println!("{:?}", new_pos);

        //now just chose one of the allowable positions at random
        self.map[chosen_i.0][chosen_i.1].t =
            Some(*new_pos.choose(&mut rand::thread_rng()).unwrap());
        //since we can only have a single position now, just give ourselves an empty superposition vector for comparision
        self.map[chosen_i.0][chosen_i.1].position = Vec::new();
        self.map[chosen_i.0][chosen_i.1].rep =
            tile_type_to_char(self.map[chosen_i.0][chosen_i.1].t.unwrap());

        println!("chose: {:?}", self.map[chosen_i.0][chosen_i.1].t.unwrap());

        //need to update neighbors positions
        while !update_me.is_empty() {
            let next = update_me.pop_front().unwrap();
            let more = self.update(next);
            for e in more {
                update_me.push_back(e);
            }
        }

        self.remaining -= 1;
    }

    //chose the tile on the board with the lowest entropy and return its coords within the map
    fn chose_tile_to_collapse(&self) -> (usize, usize) {
        return self
            .map
            .iter()
            .flatten()
            .max_by(|x, y| x.entropy().cmp(&y.entropy()))
            .unwrap()
            .coords;
    }

    //returns a neighbors struct, with inidces into the map of the neighboring tiles
    fn get_neighbors(&self, pos: (usize, usize)) -> neighbors {
        let mut n = neighbors::new();

        //north
        if self.map.get(((pos.0 as i32) - 1) as usize).is_some() {
            //n.push(&self.map[pos.0 - 1][pos.1]);
            n.north = Some(self.map[pos.0 - 1][pos.1].coords);
        }
        //south
        if self.map.get((pos.0 + 1) as usize).is_some() {
            n.south = Some(self.map[pos.0 + 1][pos.1].coords);
        }
        //west
        if self.map[pos.0].get(((pos.1 as i32) - 1) as usize).is_some() {
            n.west = Some(self.map[pos.0][pos.1 - 1].coords);
        }
        //east
        if self.map[pos.0].get(((pos.1 as i32) + 1) as usize).is_some() {
            n.east = Some(self.map[pos.0][pos.1 + 1].coords);
        }

        return n;
    }

    //take a tile with a position, update all its neighbors positions, and return a vec of coords that have been modified
    fn update(&mut self, tile: (usize, usize)) -> Vec<(usize, usize)> {
        let mut needs_update: Vec<(usize, usize)> = Vec::new();

        let neighbors = self.get_neighbors(tile);

        //if there is a north tile
        if neighbors.north.is_some() {
            let init_pos_length = self.map[neighbors.north.unwrap().0][neighbors.north.unwrap().1]
                .position
                .len();
            //trim the north tile' position down to be only the states allowable by our core tile position
            for position_possibility in &self.map[tile.0][tile.1].position.clone() {
                //retain only position possibilities in the neighbor which are valid under the main point's position
                self.map[neighbors.north.unwrap().0][neighbors.north.unwrap().1]
                    .position
                    .retain(|e| self.rules[&position_possibility][&dir::NORTH].contains(e));
            }
            let final_pos_length = self.map[neighbors.north.unwrap().0][neighbors.north.unwrap().1]
                .position
                .len();

            //if the neighbor has been reduced down to a single position, give it a type and update the struct counter
            if final_pos_length == 1 {
                let finalized_type =
                    self.map[neighbors.north.unwrap().0][neighbors.north.unwrap().1].position[0];
                self.map[neighbors.north.unwrap().0][neighbors.north.unwrap().1].t =
                    Some(finalized_type);

                println!(
                    "{},{} has been finalized to: {:?}",
                    neighbors.north.unwrap().0,
                    neighbors.north.unwrap().1,
                    finalized_type
                );
                self.remaining -= 1;
            } else if init_pos_length != final_pos_length {
                needs_update.push(neighbors.north.unwrap());
            }
        }

        //south
        if neighbors.south.is_some() {
            let init_pos_length = self.map[neighbors.south.unwrap().0][neighbors.south.unwrap().1]
                .position
                .len();
            //trim the north tile' position down to be only the states allowable by our core tile position
            for position_possibility in &self.map[tile.0][tile.1].position.clone() {
                //retain only position possibilities in the neighbor which are valid under the main point's position
                self.map[neighbors.south.unwrap().0][neighbors.south.unwrap().1]
                    .position
                    .retain(|e| self.rules[&position_possibility][&dir::SOUTH].contains(e));
            }
            let final_pos_length = self.map[neighbors.south.unwrap().0][neighbors.south.unwrap().1]
                .position
                .len();

            if final_pos_length == 1 {
                let finalized_type =
                    self.map[neighbors.south.unwrap().0][neighbors.south.unwrap().1].position[0];
                self.map[neighbors.south.unwrap().0][neighbors.south.unwrap().1].t =
                    Some(finalized_type);
                println!(
                    "{},{} has been finalized to: {:?}",
                    neighbors.south.unwrap().0,
                    neighbors.south.unwrap().1,
                    finalized_type
                );
                self.remaining -= 1;
            } else if init_pos_length != final_pos_length {
                needs_update.push(neighbors.south.unwrap());
            }
        }

        //east
        if neighbors.east.is_some() {
            let init_pos_length = self.map[neighbors.east.unwrap().0][neighbors.east.unwrap().1]
                .position
                .len();
            //trim the north tile' position down to be only the states allowable by our core tile position
            for position_possibility in &self.map[tile.0][tile.1].position.clone() {
                //retain only position possibilities in the neighbor which are valid under the main point's position
                self.map[neighbors.east.unwrap().0][neighbors.east.unwrap().1]
                    .position
                    .retain(|e| self.rules[&position_possibility][&dir::EAST].contains(e));
            }
            let final_pos_length = self.map[neighbors.east.unwrap().0][neighbors.east.unwrap().1]
                .position
                .len();

            if final_pos_length == 1 {
                let finalized_type =
                    self.map[neighbors.east.unwrap().0][neighbors.east.unwrap().1].position[0];
                self.map[neighbors.east.unwrap().0][neighbors.east.unwrap().1].t =
                    Some(finalized_type);
                println!(
                    "{},{} has been finalized to: {:?}",
                    neighbors.east.unwrap().0,
                    neighbors.east.unwrap().1,
                    finalized_type
                );
                self.remaining -= 1;
            } else if init_pos_length != final_pos_length {
                needs_update.push(neighbors.east.unwrap());
            }
        }

        //west
        if neighbors.west.is_some() {
            let init_pos_length = self.map[neighbors.west.unwrap().0][neighbors.west.unwrap().1]
                .position
                .len();
            //trim the north tile' position down to be only the states allowable by our core tile position
            for position_possibility in &self.map[tile.0][tile.1].position.clone() {
                //retain only position possibilities in the neighbor which are valid under the main point's position
                self.map[neighbors.west.unwrap().0][neighbors.west.unwrap().1]
                    .position
                    .retain(|e| self.rules[&position_possibility][&dir::WEST].contains(e));
            }
            let final_pos_length = self.map[neighbors.west.unwrap().0][neighbors.west.unwrap().1]
                .position
                .len();

            if final_pos_length == 1 {
                let finalized_type =
                    self.map[neighbors.west.unwrap().0][neighbors.west.unwrap().1].position[0];
                self.map[neighbors.west.unwrap().0][neighbors.west.unwrap().1].t =
                    Some(finalized_type);
                println!(
                    "{},{} has been finalized to: {:?}",
                    neighbors.west.unwrap().0,
                    neighbors.west.unwrap().1,
                    finalized_type
                );
                self.remaining -= 1;
            } else if init_pos_length != final_pos_length {
                needs_update.push(neighbors.west.unwrap());
            }
        }

        return needs_update;
    }
}

#[allow(non_camel_case_types)]
struct neighbors {
    north: Option<(usize, usize)>,
    south: Option<(usize, usize)>,
    east: Option<(usize, usize)>,
    west: Option<(usize, usize)>,
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
        if self.t.is_some() {
            return 0;
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
