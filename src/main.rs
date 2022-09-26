use colored::Colorize;
use rand::seq::SliceRandom;
use std::collections::HashMap;
use std::collections::VecDeque;
use std::ops::Index;
use std::ops::IndexMut;
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

            //funky casting to allow us to deal with row 0
            /*if in_board.get((row as i32 - 1) as usize).is_some() {
                //modify the hashmap entry for the NORTH direction
                cur.entry(dir::NORTH).and_modify(|vec| {
                    let t = char_to_tile_type(in_board[row - 1][col]);
                    //only add this to the rules if it is not already in the rules
                    if !vec.contains(&t) {
                        vec.push(t);
                    }
                });
            }*/

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
            /*if in_board.get(row + 1).is_some() {
                cur.entry(dir::SOUTH).and_modify(|vec| {
                    let t = char_to_tile_type(in_board[row + 1][col]);
                    if !vec.contains(&t) {
                        vec.push(t);
                    }
                });
            }*/

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
            /*if in_board[row].get(((col as i32) - 1) as usize).is_some() {
                cur.entry(dir::WEST).and_modify(|vec| {
                    let t = char_to_tile_type(in_board[row][col - 1]);
                    if !vec.contains(&t) {
                        vec.push(t);
                    }
                });
            }*/

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
            /*if in_board[row].get(col + 1).is_some() {
                cur.entry(dir::EAST).and_modify(|vec| {
                    let t = char_to_tile_type(in_board[row][col + 1]);
                    if !vec.contains(&t) {
                        vec.push(t);
                    }
                });
            }*/

            board.map[row].push(tile::fresh((row, col)));
        }
        print!("\n");
    }

    for (k, v) in &board.rules {
        println!("{:?}: {:?}", k, v);
    }

    while board.remaining > 0 {
        board.collapse();
        for row in &board.map {
            for c in row {
                print!("{}", c.rep);
            }
            print!("\n")
        }
    }

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
    fn new(size: usize, rules: HashMap<tile_type, HashMap<dir, Vec<tile_type>>>) -> Self {
        Self {
            map: Vec::new(),
            remaining: size,
            rules,
        }
    }

    //collapse a tile's position down to a single position, and update all its neighbors positions ~~recursively~~
    fn collapse(&mut self) {
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

        //queue of tiles that need to be updated
        let mut update_me: VecDeque<(usize, usize)> = VecDeque::new();

        //TODO: implement an iterator for me to clean up the code below it
        let neighbors = self.get_neighbors(chosen_i);

        //north
        if neighbors.north.is_some() {
            //update_me.push_back(neighbors.north.unwrap());
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
            //update_me.push_back(neighbors.east.unwrap());
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
            //update_me.push_back(neighbors.south.unwrap());
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
            //update_me.push_back(neighbors.west.unwrap());
            let t = &self.map[neighbors.west.unwrap().0][neighbors.west.unwrap().1];
            for possibility in &t.position {
                for t in &self.rules[possibility][&dir::EAST] {
                    if !new_pos.contains(t) {
                        new_pos.push(t.clone());
                    }
                }
            }
        }

        //now just chose one of the allowable positions at random
        self.map[chosen_i.0][chosen_i.1].t =
            Some(*new_pos.choose(&mut rand::thread_rng()).unwrap());
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

        //need to update neighbors positions
        /*while !update_me.is_empty() {
            println!("\n{} {update_me:?}", "update q is: ".green());

            let next = update_me.pop_front().unwrap();
            let more = self.update(next);
            /*for e in more {
                update_me.push_back(e);
            }*/
        }*/
        self.update(chosen_i);

        self.remaining -= 1;
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

    //take a tile with a position, update all its neighbors positions, and return a vec of coords that have been modified
    /*fn update(&mut self, tile: (usize, usize)) -> Vec<(usize, usize)> {
        println!(
            "{}: {tile:?}, which is in position: {:?}",
            "updating: ".green(),
            self.map[tile.0][tile.1].position
        );

        let mut needs_update: Vec<(usize, usize)> = Vec::new();

        let neighbors = self.get_neighbors(tile);

        //if there is a north tile and its not concrete
        if neighbors.north.is_some()
            && self.map[neighbors.north.unwrap().0][neighbors.north.unwrap().1]
                .t
                .is_none()
        {
            println!(
                "{}{:?}",
                "north starts at: ".yellow(),
                self.map[neighbors.north.unwrap().0][neighbors.north.unwrap().1]
            );
            let init_pos_length = self.map[neighbors.north.unwrap().0][neighbors.north.unwrap().1]
                .position
                .len();
            //trim the north tile' position down to be only the states allowable by our core tile position

            //for every position in our main tile's superposition
            for position_possibility in &self.map[tile.0][tile.1].position.clone() {
                //retain only position possibilities in the neighbor which are valid under this position
                self.map[neighbors.north.unwrap().0][neighbors.north.unwrap().1]
                    .position
                    //reatin elements e s.t. rules[main's subposition][north] contains e
                    .retain(|e| self.rules[&position_possibility][&dir::NORTH].contains(e));
                /*self.map[neighbors.north.unwrap().0][neighbors.north.unwrap().1].position = self
                .map[neighbors.north.unwrap().0][neighbors.north.unwrap().1]
                .position
                .iter()
                .filter(|e| !self.rules[position_possibility][&dir::NORTH].contains(e))
                .map(|v| *v)
                .collect();*/
            }
            let final_pos_length = self.map[neighbors.north.unwrap().0][neighbors.north.unwrap().1]
                .position
                .len();

            println!(
                "{}{:?}",
                "north ends up at: ".yellow(),
                self.map[neighbors.north.unwrap().0][neighbors.north.unwrap().1]
            );

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
        if neighbors.south.is_some()
            && self.map[neighbors.south.unwrap().0][neighbors.south.unwrap().1]
                .t
                .is_none()
        {
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

            println!(
                "{}{:?}",
                "south ends up at: ".yellow(),
                self.map[neighbors.south.unwrap().0][neighbors.south.unwrap().1]
            );

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
        if neighbors.east.is_some()
            && self.map[neighbors.east.unwrap().0][neighbors.east.unwrap().1]
                .t
                .is_none()
        {
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

            println!(
                "{}{:?}",
                "east ends up at: ".yellow(),
                self.map[neighbors.east.unwrap().0][neighbors.east.unwrap().1]
            );

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
        if neighbors.west.is_some()
            && self.map[neighbors.west.unwrap().0][neighbors.west.unwrap().1]
                .t
                .is_none()
        {
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

            println!(
                "{}{:?}",
                "west ends up at: ".yellow(),
                self.map[neighbors.west.unwrap().0][neighbors.west.unwrap().1]
            );

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

        /*println!(
            "{} {:?}",
            "new position is: ".green(),
            self.map[tile.0][tile.1].position
        );*/

        return needs_update;
    }*/

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
            println!("\n{}{:?}", "updating: ".green(), neighbor.tile);
            println!(
                "{} {:?}",
                "initial position".green(),
                self[neighbor.tile].position
            );
            let mut new_position = self[neighbor.tile].position.clone();
            new_position.retain(|e| self.rules[&concrete_type][&neighbor.direction].contains(e));
            self[neighbor.tile].position = new_position;

            if self[neighbor.tile].position.len() == 1 {
                self[neighbor.tile].t = Some(self[neighbor.tile].position[0]);
            }

            println!(
                "{} {:?}",
                "final position".green(),
                self[neighbor.tile].position
            );
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

#[derive(PartialEq, Hash, Eq, Debug)]
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
