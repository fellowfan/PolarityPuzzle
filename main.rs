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

    if j-1 >= 0 && board[i].as_bytes()[j-1] == ch[0]{
        return false;
    }
    else if i-1 >= 0 && board[i-1].as_bytes()[j] == ch[0]{
        return false;
    }
    else if i-1 >= 0 && board[i-1].as_bytes()[j+1] == ch[1]{
        return false;
    }
    else if j+2 < pattern[0].len() && board[i].as_bytes()[j+2] == ch[1]{
        return false;
    }
    
    true
}

fn can_vertical(board: & [&str], i: usize, j: usize, pattern: &str) -> bool
{
    let ch: Vec<char> = pattern.chars().collect();

    if j-1 >= 0 && board[i].as_bytes()[j-1] == ch[0]{
        return false;
    }   
    else if i-1 >= 0 && board[i-1].as_bytes()[j] == ch[0]{
        return false;
    }
    else if j+1 < pattern[0].len() && board[i].as_bytes()[j+1] == ch[0]{
        return false;
    }
    
    true
}
    
fn check_specs(board: & [&str], specs: & (Vec<i32>, Vec<i32>, Vec<i32>, Vec<i32>)) -> bool
{
    
}

fn solve_puzzle(board: & [&str], i: usize, j: usize, specs: & (Vec<i32>, Vec<i32>, Vec<i32>, Vec<i32>)) -> bool
{
    
}
#[cfg(test)]
#[path = "tests.rs"]
mod tests;

