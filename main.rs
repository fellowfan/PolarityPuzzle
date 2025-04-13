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

fn canHorizontal(board: & [&str], i: usize, j: usize, pattern: &str) -> bool
{
    if j-1 >= 0 && rules[i][j-1] == pattern[0]{
        return false;
    }
    else if i-1 >= 0 && rules[i-1][j] == pattern[0]{
        return false;
    }
    else if i-1 >= 0 && rules[i-1][j+1] == pattern[1]{
        return false;
    }
    else if j+2 < pattern[0].len() && rules[i][j+2] == pattern[1]{
        return false;
    }
    
    return true;
}

fn canVertical(board: & [&str], i: usize, j: usize, pattern: &str) -> bool
{
    if j-1 >= 0 && rules[i][j-1] == pattern[0]{
        return false;
    }
    else if i-1 >= 0 && rules[i-1][j] == pattern[0]{
        return false;
    }
    else if j+1 < pattern[0].len() && rules[i][j+1] == pattern[0]{
        return false;
    }
    
    return true;
}
    
fn checkSpecs(board: & [&str], specs: & (Vec<i32>, Vec<i32>, Vec<i32>, Vec<i32>)) -> bool
{
    
}

fn solvePuzzle(board: & [&str], i: usize, j: usize, specs: & (Vec<i32>, Vec<i32>, Vec<i32>, Vec<i32>)) -> bool
{
    
}
#[cfg(test)]
#[path = "tests.rs"]
mod tests;

