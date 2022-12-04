use std::error::Error;
use std::iter::repeat;
use std::iter::zip;

use std::collections::VecDeque;

use rand::distributions::Standard;
use rand::prelude::*;

use elefren::helpers::cli;
use elefren::helpers::toml;
use elefren::prelude::*;

use argh::FromArgs;

#[derive(Clone, Copy, Debug, PartialEq)]
struct Pixel(usize, usize);

impl Pixel {
    fn new(x: usize, y: usize) -> Self {
        Self(x, y)
    }

    /// Compass neighbors that are valid Pixels (i.e. not negative)
    fn neighbors(&self) -> Vec<Self> {
        match (self.0, self.1) {
            (0, 0) => vec![Self(1, 0), Self(0, 1)],
            (0, y) => vec![Self(1, 0), Self(0, 1), Self(0, y - 1)],
            (x, 0) => vec![Self(x + 1, 0), Self(x, 1), Self(x - 1, 0)],
            (x, y) => vec![
                Self(x + 1, y),
                Self(x - 1, y),
                Self(x, y - 1),
                Self(x, y + 1),
            ],
        }
    }
}

/// A board is represented by a bunch characters with row-major order, i.e. a 3x3 board is
/// represented like: [(0, 0), (0, 1), (0, 2), (1, 0), ... ]
/// A 'x' character represents a wall, and any other
/// character is a space (although it is typically a '.', sometimes during processing we make
/// notes "on the board" using characters that aren't '.'
/// TODO: this representation is bad and it should feel bad
struct Board {
    rows: usize,
    cols: usize,
    board: Vec<char>,
}

impl Board {
    /// Make a new board that is completely empty.  Currently not used.
    #[allow(dead_code)]
    pub fn new(rows: usize, cols: usize) -> Self {
        let mut board = Vec::with_capacity(rows * cols);
        board.resize(rows * cols, '.');
        Self { rows, cols, board }
    }

    /// Makes a board with a waffle layout, which is like a bunch of walls
    /// with one-space open spaces.
    /// i.e. rows = 5 and cols = 5:
    ///  xxxxx
    ///  x.x.x
    ///  xxxxx
    ///  x.x.x
    ///  xxxxx
    ///
    /// Panics if rows and/or cols are not odd.
    fn make_waffle(rows: usize, cols: usize) -> Self {
        if rows % 2 != 1 || cols % 2 != 1 {
            panic!("rows and cols must be odd to make a waffle board");
        }

        let mut solid_wall = Vec::with_capacity(cols);
        solid_wall.resize(cols, 'x');
        let mut openings_wall = Vec::with_capacity(cols);
        for idx in 0..cols {
            openings_wall.push(match idx % 2 {
                0 => 'x',
                1 => '.',
                _ => unreachable!(),
            });
        }
        let mut board = Vec::with_capacity(rows);
        for row in 0..rows {
            match row % 2 {
                0 => board.append(&mut solid_wall.clone()),
                1 => board.append(&mut openings_wall.clone()),
                _ => unreachable!(),
            }
        }
        Self { rows, cols, board }
    }

    /// Lookup the index in the board vector where the x,y pixel is.
    fn index(&self, x: usize, y: usize) -> Option<usize> {
        if x >= self.rows || y >= self.cols {
            return None;
        }
        Some(x * self.cols + y)
    }

    /// Returns true if the pixel is on the board.
    fn inbounds(&self, idx: Pixel) -> bool {
        idx.0 < self.rows && idx.1 < self.cols
    }

    /// Returns the character at (x, y) if it's on the board, or None otherwise.
    fn at(&self, x: usize, y: usize) -> Option<char> {
        self.index(x, y)
            .and_then(|idx| self.board.get(idx).copied())
    }

    /// Same as at, but for Pixels.
    fn at_p(&self, idx: Pixel) -> Option<char> {
        self.at(idx.0, idx.1)
    }

    /// Sets the pixel at `idx` to `c`.
    fn set(&mut self, idx: Pixel, c: char) -> &mut Self {
        let idx = self.index(idx.0, idx.1).unwrap();
        *self.board.get_mut(idx).unwrap() = c;
        self
    }

    /// Empties the spot on the board at (x, y), setting it to '.'
    fn empty(&mut self, idx: Pixel) -> &mut Self {
        self.set(idx, '.')
    }

    /// Fills all empte spaces connected to `start` with `c`.
    fn flood_fill(&mut self, start: Pixel, c: char) -> &mut Self {
        let mut queue = vec![start];
        while let Some(pixel) = queue.pop() {
            if !self.inbounds(pixel) || self.at_p(pixel).unwrap() != '.' {
                continue;
            }
            let _ = self.set(pixel, c);
            queue.append(&mut pixel.neighbors());
        }
        self
    }

    fn idx_to_pixel(&self, idx: usize) -> Pixel {
        Pixel::new(idx / self.cols, idx % self.cols)
    }

    /// Finds all occurrences of `ch` on the board, returning their positions.
    fn find(&self, ch: char) -> Vec<Pixel> {
        self.board
            .iter()
            .enumerate()
            .filter_map(|(i, c)| {
                if *c == ch {
                    Some(self.idx_to_pixel(i))
                } else {
                    None
                }
            })
            .collect()
    }

    /// Finds all blank spots on the board, returning their positions.
    fn blanks(&self) -> Vec<Pixel> {
        self.find('.')
    }

    fn empty_non_walls(&mut self) {
        let board = std::mem::replace(&mut self.board, Vec::new());
        self.board = board
            .into_iter()
            .map(|x| (x == 'x').then_some('x').unwrap_or('.'))
            .collect();
    }

    fn as_emoji(&self, tiles: Tileset) -> String {
        let mut rng = rand::thread_rng();
        let emoji_board: Vec<char> = self
            .board
            .iter()
            .map(|x| {
                if x != &'x' {
                    '🌕'
                } else {
                    tiles.pick(&mut rng)
                }
            })
            .collect();
        let mut res = String::new();
        for row in 0..self.rows {
            let start = self.index(row, 0).unwrap();
            let end = self.index(row, self.cols - 1).unwrap();
            res.push_str(
                format!("{}\n", &emoji_board[start..=end].iter().collect::<String>()).as_str(),
            );
        }
        res
    }

    /// This algorithm makes some mazes with unfortunate rotational symmetry.
    /// Returns a 'vane score', which is 12 or 16 based on whether there are three
    /// or four paths from here that take two steps, then a right turn, and at least two
    /// steps.
    /// Currently only works on a board where empty spaces are '.'
    fn has_unfortunate_rotational_symmetry(&self) -> usize {
        let blanks = self.blanks();
        let in_blanks = |p: &Pixel| blanks.iter().find(|px| p == *px).is_some();
        self.blanks()
            .clone()
            .into_iter()
            .find_map(|p| {
                if p.0 < 2 || p.1 < 2 {
                    return None;
                }
                let vanes_counts: Vec<_> = [
                    [
                        Pixel(p.0 - 1, p.1),
                        Pixel(p.0 - 2, p.1),
                        Pixel(p.0 - 2, p.1 + 1),
                        Pixel(p.0 - 2, p.1 + 2),
                    ],
                    [
                        Pixel(p.0 + 1, p.1),
                        Pixel(p.0 + 2, p.1),
                        Pixel(p.0 + 2, p.1 - 1),
                        Pixel(p.0 + 2, p.1 - 2),
                    ],
                    [
                        Pixel(p.0, p.1 - 1),
                        Pixel(p.0, p.1 - 2),
                        Pixel(p.0 - 1, p.1 - 2),
                        Pixel(p.0 - 2, p.1 - 2),
                    ],
                    [
                        Pixel(p.0, p.1 + 1),
                        Pixel(p.0, p.1 + 2),
                        Pixel(p.0 + 1, p.1 + 2),
                        Pixel(p.0 + 2, p.1 + 2),
                    ],
                ]
                .into_iter()
                .map(|vane| {
                    let vane_count = vane.iter().filter(|p| in_blanks(*p)).count();
                    (vane_count > 3).then_some(vane_count).unwrap_or(0)
                })
                .collect();
                println!("{vanes_counts:?}");
                let score = vanes_counts.iter().sum();
                (score >= 12).then_some(score)
            })
            .unwrap_or(0)
    }
}

impl core::fmt::Display for Board {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        writeln!(f, "Board with ({}, {}):", self.rows, self.cols)?;
        for row in 0..self.rows {
            let start = self.index(row, 0).unwrap();
            let end = self.index(row, self.cols - 1).unwrap();
            writeln!(f, "{}", &self.board[start..=end].iter().collect::<String>())?;
        }
        Ok(())
    }
}

fn start_end_spots(cols: usize, rows: usize) -> Vec<Pixel> {
    let top_bottom_idx = (0..cols).filter(|x| x % 2 == 1);
    let left_right_idx = (0..rows).filter(|x| x % 2 == 1);

    let mut v: Vec<(usize, usize)> = zip(repeat(0), top_bottom_idx.clone()).collect();
    v.append(&mut zip(repeat(rows - 1), top_bottom_idx.clone()).collect());
    v.append(&mut zip(left_right_idx.clone(), repeat(0)).collect());
    v.append(&mut zip(left_right_idx.clone(), repeat(cols - 1)).collect());
    v.into_iter().map(|(x, y)| Pixel::new(x, y)).collect()
}

fn open_startend(b: &mut Board) {
    let mut spots = start_end_spots(b.rows, b.cols);

    let mut rng = rand::thread_rng();
    let start = spots.choose(&mut rng).unwrap().clone();

    // The end should be on a different wall
    spots.retain(|p| (p.0 != start.0) && (p.1 != start.1));

    let end = spots.choose(&mut rng).unwrap().clone();

    println!("Start spots: {start:?} {end:?}");

    b.empty(start).empty(end);
}

fn fill_trees(b: &mut Board, count: usize) {
    // If we would fill with the magic wall character, don't.
    if count == ('x' as usize) {
        fill_trees(b, count + 1);
    }

    let Some(p) = b.blanks().first().cloned() else {
        return;
    };

    b.flood_fill(p, char::from_u32(count.try_into().unwrap()).unwrap());
    fill_trees(b, count + 1);
}

#[derive(Clone, Copy, Debug)]
struct Crossing {
    point: Pixel,
    from: char,
    to: char,
}

impl Crossing {
    fn reconcile(mut self, from: char, to: char) -> Option<Self> {
        if (self.from == from && self.to == to) || (self.from == to && self.to == from) {
            return None;
        }
        if self.to == from {
            self.to = to;
        } else if self.from == from {
            self.from = to;
        }
        Some(self)
    }
}

/// Finds places where a wall can be knocked out. These are places where a two non-walls are separated by
/// one wall.
/// Crossings find a wall point, and pick one side and the other side to map a direction to poke
/// the wall.
fn find_crossings(b: &Board) -> VecDeque<Crossing> {
    let exes = b.find('x');
    exes.into_iter()
        .map(|point| {
            let tb = (point.0 > 0).then(|| {
                match (b.at(point.0 - 1, point.1), b.at(point.0 + 1, point.1)) {
                    (Some(from), Some(to)) if from != 'x' && to != 'x' => {
                        Some(Crossing { point, from, to })
                    }
                    _ => None,
                }
            });
            let lr = (point.1 > 0).then(|| {
                match (b.at(point.0, point.1 - 1), b.at(point.0, point.1 + 1)) {
                    (Some(from), Some(to)) if from != 'x' && to != 'x' => {
                        Some(Crossing { point, from, to })
                    }
                    _ => None,
                }
            });
            vec![tb, lr]
        })
        .flatten()
        .flatten()
        .filter_map(|x| x)
        .collect()
}

#[derive(Debug, Copy, Clone)]
enum Tileset {
    Garden,
    City,
    // Winter is for only part of the year, and not always used.
    #[allow(dead_code)]
    Winter,
    // Spoopy is for near holidays and not always used.
    #[allow(dead_code)]
    Spoopy,
    Moons,
}

impl Tileset {
    // Mushroom, Tree, Evergreen,, Sunflower, Fountain, Tree x 4, Evergreen x 4
    const GARDEN_TILES: &'static [char] = &[
        '🍄', '🌳', '🌲', '🌻', '⛲', '🌳', '🌳', '🌲', '🌲', '🌳', '🌳', '🌲', '🌲',
    ];
    // Snowman, Yule Tree x2, Evergreen x2, Gift, Mountain x2, Snowflake x2
    const WINTER_TILES: &'static [char] = &['☃', '🎄', '🎄', '🌲', '🌲', '🎁', '🏔', '🏔', '❄', '❄'];
    // Just a moon
    const MOON_TILES: &'static [char] = &['🌑'];
    // School, Office, Hospital, Bank, Hotel, Euro Post Office, Department Store, Factory,
    // Construction, Cityscape
    const CITY_TILES: &'static [char] = &['🏫', '🏢', '🏥', '🏦', '🏨', '🏤', '🏬', '🏭', '🏗', '🏙'];
    // Corn, Spider, Jack o lantern, Bath,  Spiderweb, Ghost, Skull, Seedling x 5
    const SPOOPY_TILES: &'static [char] = &[
        '🌽', '🕷', '🎃', '🦇', '🕸', '👻', '💀', '🌱', '🌱', '🌱', '🌱', '🌱',
    ];

    fn pick<R: Rng + ?Sized>(&self, rng: &mut R) -> char {
        let tiles = match self {
            Self::Winter => Self::WINTER_TILES,
            Self::Garden => Self::GARDEN_TILES,
            Self::City => Self::CITY_TILES,
            Self::Spoopy => Self::SPOOPY_TILES,
            Self::Moons => Self::MOON_TILES,
        };

        *(tiles.choose(rng).unwrap())
    }
}

impl Distribution<Tileset> for Standard {
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> Tileset {
        match rng.gen_range(0..=3) {
            // rand 0.8
            0 => Tileset::Garden,
            1 => Tileset::City,
            2 => Tileset::Winter,
            _ => Tileset::Moons,
        }
    }
}

fn cli_register() -> Result<Mastodon, Box<dyn Error>> {
    use elefren::scopes::*;
    let scopes = Scopes::write(Write::Statuses).and(Scopes::read_all());

    let reg = Registration::new("https://botsin.space")
        .client_name("tinymaze.rs")
        .scopes(scopes)
        .build()?;

    let mastodon = cli::authenticate(reg)?;

    toml::to_file(&*mastodon, "mastodon.toml")?;

    Ok(mastodon)
}

/// Make some random tiny mazes.
#[derive(FromArgs)]
struct Options {
    /// turn on to post to a Mastodon server
    #[argh(switch, short = 'p')]
    post: bool,
}

fn main() -> Result<(), Box<dyn Error>> {
    let options: Options = argh::from_env();
    let mut board = Board::make_waffle(11, 11);

    let tileset = rand::random();

    while {
        open_startend(&mut board);
        fill_trees(&mut board, 'A' as usize);
        let mut crossings = find_crossings(&board);
        let mut rng = rand::thread_rng();
        crossings.make_contiguous().shuffle(&mut rng);
        while let Some(crossing) = crossings.pop_front() {
            board.set(crossing.point, crossing.to);
            crossings = crossings
                .into_iter()
                .filter_map(|x| x.reconcile(crossing.from, crossing.to))
                .collect();
        }

        board.empty_non_walls();
        let score = board.has_unfortunate_rotational_symmetry();
        if score >= 12 {
            let toot = board.as_emoji(tileset);
            println!("Unfortunately, {score} rotationally:");
            println!("{toot}");
        }
        score >= 12
    }
    /* reset the board */
    {
        board = Board::make_waffle(11, 11);
    }

    let toot = board.as_emoji(tileset);

    if options.post {
        let mastodon = match toml::from_file("mastodon.toml") {
            Ok(data) => Mastodon::from(data),
            Err(_) => cli_register()?,
        };

        let status = StatusBuilder::new()
            .status(toot.clone())
            .visibility(elefren::status_builder::Visibility::Public)
            .build()?;

        let _posted = mastodon.new_status(status)?;

        println!("Posted:");
    }
    println!("{toot}");

    Ok(())
}
