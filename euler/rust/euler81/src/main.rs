use std::cmp::min;
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;
use std::path::Path;

fn main() {
    let mut grid = Vec::new();
    let path = Path::new("p081_matrix.txt");
    let file = File::open(&path).unwrap();
    let lines = BufReader::new(file).lines();
    let mut n = 0;
    for line in lines {
        if let Ok(data) = line {
            let mut row = Vec::new();
            for item in data.split(',') {
                row.push(item.parse::<u32>().unwrap());
            }
            if n == 0 {
                n = row.len();
            } else {
                assert!(row.len() == n);
            }
            grid.push(row);
        }
    }
    assert!(grid.len() == n);
    eprintln!("Read {} by {} grid", n, n);

    for i in (0..(n-1)).rev() {
        grid[n - 1][i] = grid[n - 1][i] + grid[n - 1][i + 1];
        grid[i][n - 1] = grid[i][n - 1] + grid[i + 1][n - 1];
    }
    for i in (0..(n-1)).rev() {
        for j in (0..=i).rev() {
            grid[i][j] = calc(&grid, i, j);
        }
        for j in (0..i).rev() {
            grid[j][i] = calc(&grid, j, i);
        }
    }
    eprintln!("{:?}", grid[0][0]);
}

fn calc(grid: &Vec<Vec<u32>>, row: usize, col: usize) -> u32 {
    let opt1 = grid[row][col + 1];
    let opt2 = grid[row + 1][col];
    grid[row][col] + min(opt1, opt2)
}
