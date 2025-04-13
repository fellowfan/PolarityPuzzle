#![allow(non_snake_case,non_camel_case_types,dead_code)]

/*
    Fill in the polarity function below. Use as many helpers as you want.
    Test your code by running 'cargo test' from the tester_rs_simple directory.
    
*/

fn polarity(board: & [&str], specs: & (Vec<i32>, Vec<i32>, Vec<i32>, Vec<i32>)) -> Vec<String>
{
    // 5x6 solution hardcoded to demonstrate return type

          vec![ String::from("+-+-X-"), 
          String::from("-+-+X+"), 
          String::from("XX+-+-"), 
          String::from("XX-+X+"), 
          String::from("-+XXX-") ]
}

fn can_horizontal(board: & [&str], i: usize, j: usize, pattern: &str) -> bool
{
    let ch: Vec<char> = pattern.chars().collect();

    if j-1 >= 0 && board[i].as_bytes()[j-1] as char == ch[0]{
        return false;
    }
    else if i-1 >= 0 && board[i-1].as_bytes()[j] as char == ch[0]{
        return false;
    }
    else if i-1 >= 0 && board[i-1].as_bytes()[j+1] as char == ch[1]{
        return false;
    }
    else if j+2 < pattern[0].len() && board[i].as_bytes()[j+2] as char == ch[1]{
        return false;
    }
    
    true
}

fn can_vertical(board: & [&str], i: usize, j: usize, pattern: &str) -> bool
{
    let ch: Vec<char> = pattern.chars().collect();

    if j-1 >= 0 && board[i].as_bytes()[j-1] as char == ch[0]{
        return false;
    }   
    else if i-1 >= 0 && board[i-1].as_bytes()[j] as char == ch[0]{
        return false;
    }
    else if j+1 < pattern[0].len() && board[i].as_bytes()[j+1] as char == ch[0]{
        return false;
    }
    
    true
}
    
fn check_specs(board: & [&str], specs: & (Vec<i32>, Vec<i32>, Vec<i32>, Vec<i32>)) -> bool
{
    // Count the constraints on the calculated board from solve_puzzle
    let mut posH = vec![0; board.len() as usize];
    let mut negH = vec![0; board.len() as usize];
    let mut posV = vec![0; board[0].len() as usize];
    let mut negV = vec![0; board[0].len() as usize];

    for row in 0..board.len() {
        let ch: Vec<char> = board[row].chars().collect();

        for col in 0..board[0].len() {
            if ch[col] == '+' {
                posH[row] += 1;
            }
            else if ch[col] == '-' {
                negH[row] += 1;
            }
        }
    }

    for col in 0..board[0].len() {
        let ch: Vec<char> = board[col].chars().collect();

        for row in 0..board.len() {
            if ch[row] == '+' {
                posV[col] += 1;
            }
            else if ch[row] == '-' {
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

fn solve_puzzle(board: & [&str], i: usize, j: usize, specs: & (Vec<i32>, Vec<i32>, Vec<i32>, Vec<i32>)) -> bool
{
    true
}
#[cfg(test)]
#[path = "tests.rs"]
mod tests;

