#![allow(non_snake_case,non_camel_case_types,dead_code)]

/*
    Fill in the polarity function below. Use as many helpers as you want.
    Test your code by running 'cargo test' from the tester_rs_simple directory.
    
*/

fn polarity(board: & [&str], specs: & (Vec<i32>, Vec<i32>, Vec<i32>, Vec<i32>)) -> Vec<String>
{
    // 5x6 solution hardcoded to demonstrate return type

    solve_puzzle(board, 0, 0, specs)
         /* vec![ String::from("+-+-X-"), 
          String::from("-+-+X+"), 
          String::from("XX+-+-"), 
          String::from("XX-+X+"), 
          String::from("-+XXX-") ] */
}

fn can_horizontal(board: &mut Vec<Vec<char>>, i: usize, j: usize, pattern: &str) -> bool
{
    let ch: Vec<char> = pattern.chars().collect();

    if j >= 1 && board[i][j-1] as char == ch[0]{
        return false;
    }
    else if i >= 1 && board[i-1][j] as char == ch[0]{
        return false;
    }
    else if i >= 1 && board[i-1][j+1] as char == ch[1]{
        return false;
    }
    else if j+2 < board[0].len() && board[i][j+2] as char == ch[1]{
        return false;
    }
    
    true
}

fn can_vertical(board: &mut Vec<Vec<char>>, i: usize, j: usize, pattern: &str) -> bool
{
    let ch: Vec<char> = pattern.chars().collect();

    if j >= 1 && board[i][j-1] as char == ch[0]{
        return false;
    }   
    else if i >= 1 && board[i-1][j] as char == ch[0]{
        return false;
    }
    else if j+1 < board[0].len() && board[i][j+1] as char == ch[0]{
        return false;
    }
    
    true
}
    
fn check_specs(board: &mut Vec<Vec<char>>, specs: & (Vec<i32>, Vec<i32>, Vec<i32>, Vec<i32>)) -> bool
{
    // Count the constraints on the calculated board from solve_puzzle
    let mut posH = vec![0; board.len() as usize];
    let mut negH = vec![0; board.len() as usize];
    let mut posV = vec![0; board[0].len() as usize];
    let mut negV = vec![0; board[0].len() as usize];

    for row in 0..board.len() {
       //  let ch: Vec<char> = board[row].chars().collect();

        for col in 0..board[0].len() {
            if board[row][col] == '+' {
                posH[row] += 1;
            }
            else if board[row][col] == '-' {
                negH[row] += 1;
            }
        }
    }

    for col in 0..board[0].len() {
       // let ch: Vec<char> = board[col].chars().collect();

        for row in 0..board.len() {
            if board[row][col] == '+' {
                posV[col] += 1;
            }
            else if board[row][col] == '-' {
                negV[col] += 1;
            }
        }
    }

    // Compare constraints found on board solution with provided solution constraints (i.e. specs)
    for row in 0..board.len() {
        if specs.0[row] != -1 && posH[row] as i32 != specs.0[row]{
            return false;
        } 
        if specs.1[row] != -1 && negH[row] as i32 != specs.1[row]{
            return false;
        } 
    }

    for col in 0..board[0].len() {
        if specs.2[col] != -1 && posV[col] as i32 != specs.2[col]{
            return false;
        } 
        if specs.3[col] != -1 && negV[col] as i32 != specs.3[col]{
            return false;
        } 
    }

    // No discrepencies found --> return true
    true
}


fn solve_puzzle(board: & [&str], i: usize, j: usize, specs: & (Vec<i32>, Vec<i32>, Vec<i32>, Vec<i32>)) -> Vec<String>
{
    let mut grid: Vec<Vec<char>> = board.iter().map(|row| row.chars().collect()).collect();

    if backtrack(&mut grid, 0, 0, specs) {
        return grid.into_iter().map(|chars| chars.into_iter().collect()).collect();
    }

    fn backtrack(grid: &mut Vec<Vec<char>>, i: usize, j: usize, specs: &(Vec<i32>, Vec<i32>, Vec<i32>, Vec<i32>)) -> bool {
        // Base case: reached end of grid
        if i >= grid.len() {
            return check_specs(grid, specs);
        }
        
        // Move to next row if at end of current row
        if j >= grid[0].len() {
            return backtrack(grid, i + 1, 0, specs);
        }
    
        match grid[i][j] {
            'L' => {
                // Must have matching 'R' to the right
                if j + 1 >= grid[0].len() || grid[i][j+1] != 'R' {
                    return backtrack(grid, i, j + 1, specs);
                }
    
                // Try +- pattern
                if can_horizontal(grid, i, j, "+-") {
                    grid[i][j] = '+';
                    grid[i][j+1] = '-';
                    if backtrack(grid, i, j + 2, specs) {
                        return true;
                    }
                    grid[i][j] = 'L';
                    grid[i][j+1] = 'R';
                }
    
                // Try -+ pattern
                if can_horizontal(grid, i, j, "-+") {
                    grid[i][j] = '-';
                    grid[i][j+1] = '+';
                    if backtrack(grid, i, j + 2, specs) {
                        return true;
                    }
                    grid[i][j] = 'L';
                    grid[i][j+1] = 'R';
                }
    
                // Try XX pattern
                grid[i][j] = 'X';
                grid[i][j+1] = 'X';
                if backtrack(grid, i, j + 2, specs) {
                    return true;
                }
                grid[i][j] = 'L';
                grid[i][j+1] = 'R';
            }
            'T' => {
                // Must have matching 'B' below
                if i + 1 >= grid.len() || grid[i+1][j] != 'B' {
                    return backtrack(grid, i, j + 1, specs);
                }
    
                // Try vertical +- pattern
                if can_vertical(grid, i, j, "+-") {
                    grid[i][j] = '+';
                    grid[i+1][j] = '-';
                    if backtrack(grid, i, j + 1, specs) {
                        return true;
                    }
                    grid[i][j] = 'T';
                    grid[i+1][j] = 'B';
                }
    
                // Try vertical -+ pattern
                if can_vertical(grid, i, j, "-+") {
                    grid[i][j] = '-';
                    grid[i+1][j] = '+';
                    if backtrack(grid, i, j + 1, specs) {
                        return true;
                    }
                    grid[i][j] = 'T';
                    grid[i+1][j] = 'B';
                }
    
                // Try XX pattern
                grid[i][j] = 'X';
                grid[i+1][j] = 'X';
                if backtrack(grid, i, j + 1, specs) {
                    return true;
                }
                grid[i][j] = 'T';
                grid[i+1][j] = 'B';
            }
            _ => {
                // Skip already filled cells
                return backtrack(grid, i, j + 1, specs);
            }
        }
    
        false
    }
    grid.into_iter().map(|chars| chars.into_iter().collect()).collect()

}
    
#[cfg(test)]
#[path = "tests.rs"]
mod tests;

