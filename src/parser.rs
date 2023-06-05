use crate::lexer::{C1Lexer, C1Token};
use crate::ParseResult;
use std::ops::{Deref, DerefMut};

pub struct C1Parser<'a>(C1Lexer<'a>);
// Implement Deref and DerefMut to enable the direct use of the lexer's methods
impl<'a> Deref for C1Parser<'a> {
   type Target = C1Lexer<'a>;

     fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'a> DerefMut for C1Parser<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
       &mut self.0
    }
}

impl<'a> C1Parser<'a> {
    pub fn parse(text: &str) -> ParseResult {
      let mut parser = Self::initialize_parser(text);
      parser.program()
    }

    fn initialize_parser(text: &str) -> C1Parser {
        C1Parser(C1Lexer::new(text))
   }
   
   fn type_(&mut self) -> ParseResult {
       match self.current_token() {
           Some(token) => {
               match token.token_type {
                   C1Token::KeywordBoolean
                   | C1Token::KeywordFloat
                   | C1Token::KeywordInt
                   | C1Token::KeywordVoid => {
                       self.eat();
                       Ok(())
                   }
                   _ => Err(format!("Expected type, found {:?}", token)),
               }
           }
           None => Err("Unexpected end of input".to_string()),
       }
   }
/// program ::= ( functiondefinition )* <EOF>

fn expect_token(&mut self, expected_token: &C1Token) -> ParseResult {
    let token = self.next_token()?;
    if token == *expected_token {
        Ok(token)
    } else {
        Err(ParseError::new(
            format!("Expected {:?}, found {:?}", expected_token, token),
            token.position,
        ))
    }
}
      
fn next_token(&mut self) -> Option<C1Token> {
    let next_token = self.tokens.next();
    if next_token.is_none() {
        Some(C1Token::EOF)
    } else {
        next_token
    }
}

fn assignment(&mut self) -> ParseResult {
    if self.current_matches(&C1Token::Identifier) {
        self.expect_token(&C1Token::Identifier)?;
        self.expect_token(&C1Token::Assign)?;
        self.assignment()?;
    } else {
        self.expr()?;
    }
    Ok(())
}

fn expr(&mut self) -> ParseResult {
    self.simpexpr()?;
    if let Some(token) = self.current_token() {
        if token == &C1Token::Equal
            || token == &C1Token::NotEqual
            || token == &C1Token::LessEqual
            || token == &C1Token::GreaterEqual
            || token == &C1Token::Less
            || token == &C1Token::Greater
        {
            self.eat();
            self.simpexpr()?;
        }
    }
    Ok(())
}

fn simpexpr(&mut self) -> ParseResult {
    if self.current_token().is_some() && self.current_token() == &C1Token::Minus {
        self.eat();
    }
    self.term()?;
    while let Some(token) = self.current_token() {
        if token == &C1Token::Plus || token == &C1Token::Minus || token == &C1Token::OrOr {
            self.eat();
            self.term()?;
        } else {
            break;
        }
    }
    Ok(())
}

fn term(&mut self) -> ParseResult {
    self.factor()?;
    while let Some(token) = self.current_token() {
        if token == &C1Token::Multiply || token == &C1Token::Divide || token == &C1Token::AndAnd {
            self.eat();
            self.factor()?;
        } else {
            break;
        }
    }
    Ok(())
}

fn factor(&mut self) -> ParseResult {
    if self.current_matches(&C1Token::ConstInt)
        || self.current_matches(&C1Token::ConstFloat)
        || self.current_matches(&C1Token::ConstBoolean)
    {
        self.eat();
    } else if self.current_matches(&C1Token::Identifier) {
        self.eat();
        if self.current_matches(&C1Token::LeftParenthesis) {
            self.expect_token(&C1Token::LeftParenthesis)?;
            self.expect_token(&C1Token::RightParenthesis)?;
        }
    } else if self.current_matches(&C1Token::LeftParenthesis) {
        self.expect_token(&C1Token::LeftParenthesis)?;
        self.assignment()?;
        self.expect_token(&C1Token::RightParenthesis)?;
    } else {
        return Err(format!("Unexpected token: {:?}", self.current_token()));
    }
    Ok(())
}   
   
fn program(&mut self) -> ParseResult {
    while self.current_token().is_some() {
        self.functiondefinition()?;
    }
    self.expect_token(&C1Token::EOF)?;
    Ok(())
}      
      
fn functiondefinition(&mut self) -> ParseResult {
    self.type_()?;
    self.expect_token(&C1Token::Identifier)?;
    self.expect_token(&C1Token::LeftParenthesis)?;
    self.expect_token(&C1Token::RightParenthesis)?;
    self.expect_token(&C1Token::LeftBrace)?;
    self.statementlist()?;
    self.expect_token(&C1Token::RightBrace)?;
    Ok(())
}

 fn statassignment(&mut self) -> ParseResult {
        self.expect_token(&C1Token::Identifier)?;
        self.expect_token(&C1Token::Assign)?;
        self.assignment()?;
        Ok(())
    }

    fn functioncall(&mut self) -> ParseResult {
        self.expect_token(&C1Token::Identifier)?;
        self.expect_token(&C1Token::LeftParenthesis)?;
        self.expect_token(&C1Token::RightParenthesis)?;
        Ok(())
    }

    fn block(&mut self) -> ParseResult {
        self.expect_token(&C1Token::LeftBrace)?;
        self.statementlist()?;
        self.expect_token(&C1Token::RightBrace)?;
        Ok(())
    }      
      
fn statementlist(&mut self) -> ParseResult {
    self.statement()?;
    if self.current_token().is_some() {
        self.statementlist()?;
    }
    Ok(())
}

fn statement(&mut self) -> ParseResult {
    if self.current_matches(&C1Token::KeywordIf) {
        self.ifstatement()?;
    } else if self.current_matches(&C1Token::KeywordReturn) {
        self.returnstatement()?;
    } else if self.current_matches(&C1Token::KeywordPrintf) {
        self.printf()?;
    } else if self.current_matches(&C1Token::Identifier) {
        if self.next_matches(&C1Token::Assign) {
            self.statassignment()?;
        } else {
            self.functioncall()?;
        }
    } else if self.current_matches(&C1Token::LeftBrace) {
        self.block()?;
    } else {
        return Err(format!("Unexpected token: {:?}", self.current_token()));
    }
    self.expect_token(&C1Token::Semicolon)?;
    Ok(())
}

fn ifstatement(&mut self) -> ParseResult {
    self.expect_token(&C1Token::KeywordIf)?;
    self.expect_token(&C1Token::LeftParenthesis)?;
    self.assignment()?;
    self.expect_token(&C1Token::RightParenthesis)?;
    self.block()?;
    Ok(())
}

fn returnstatement(&mut self) -> ParseResult {
    self.expect_token(&C1Token::KeywordReturn)?;
    if !self.current_matches(&C1Token::Semicolon) {
        self.assignment()?;
    }
    Ok(())
}

fn printf(&mut self) -> ParseResult {
    self.expect_token(&C1Token::KeywordPrintf)?;
    self.expect_token(&C1Token::LeftParenthesis)?;
    self.assignment()?;
    self.expect_token(&C1Token::RightParenthesis)?;
    Ok(())
}

/// Check whether the current token is equal to the given token. If yes, consume it, otherwise
/// return an error with the given error message
fn check_and_eat_token(&mut self, token: &C1Token, error_message: &str) -> ParseResult {
    if self.current_matches(token) {
        self.eat();
        Ok(())
    } else {
        Err(String::from(error_message))
    }
}

/// For each token in the given slice, check whether the token is equal to the current token,
/// consume the current token, and check the next token in the slice against the next token
/// provided by the lexer.
fn check_and_eat_tokens(&mut self, token: &[C1Token], error_message: &str) -> ParseResult {
    match token
        .iter()
        .map(|t| self.check_and_eat_token(t, error_message))
        .filter(ParseResult::is_err)
        .last()
    {
        None => Ok(()),
        Some(err) => err,
    }
}

/// Check whether the given token matches the current token
fn current_matches(&self, token: &C1Token) -> bool {
    match &self.current_token() {
        None => false,
        Some(current) => current == token,
    }
}

/// Check whether the given token matches the next token
fn next_matches(&self, token: &C1Token) -> bool {
    match &self.peek_token() {
        None => false,
        Some(next) => next == token,
    }
}

/// Check whether any of the tokens matches the current token.
fn any_match_current(&self, token: &[C1Token]) -> bool {
    token.iter().any(|t| self.current_matches(t))
}

/// Check whether any of the tokens matches the current token, then consume it
fn any_match_and_eat(&mut self, token: &[C1Token], error_message: &String) -> ParseResult {
    if token
        .iter()
        .any(|t| self.check_and_eat_token(t, "").is_ok())
    {
        Ok(())
    } else {
        Err(String::from(error_message))
    }
}

fn error_message_current(&self, reason: &'static str) -> String {
    match self.current_token() {
        None => format!("{}. Reached EOF", reason),
        Some(_) => format!(
            "{} at line {:?} with text: '{}'",
            reason,
            self.current_line_number().unwrap(),
            self.current_text().unwrap()
        ),
    }
}

fn error_message_peek(&mut self, reason: &'static str) -> String {
    match self.peek_token() {
       None => format!("{}. Reached EOF", reason),
        Some(_) => format!(
           "{} at line {:?} with text: '{}'",
           reason,
            self.peek_line_number().unwrap(),
            self.peek_text().unwrap()
        ),
        }
    }
}

// #[cfg(test)]
// mod tests {
//     use crate::parser::{C1Parser, ParseResult};
//
//     fn call_method<'a, F>(parse_method: F, text: &'static str) -> ParseResult
//     where
//         F: Fn(&mut C1Parser<'a>) -> ParseResult,
//     {
//         let mut parser = C1Parser::initialize_parser(text);
//         if let Err(message) = parse_method(&mut parser) {
//             eprintln!("Parse Error: {}", message);
//             Err(message)
//         } else {
//             Ok(())
//         }
//     }
//
//     #[test]
//     fn parse_empty_program() {
//         let result = C1Parser::parse("");
//         assert_eq!(result, Ok(()));
//
//         let result = C1Parser::parse("   ");
//         assert_eq!(result, Ok(()));
//
//         let result = C1Parser::parse("// This is a valid comment!");
//         assert_eq!(result, Ok(()));
//
//         let result = C1Parser::parse("/* This is a valid comment!\nIn two lines!*/\n");
//         assert_eq!(result, Ok(()));
//
//         let result = C1Parser::parse("  \n ");
//         assert_eq!(result, Ok(()));
//     }
//
//     #[test]
//     fn fail_invalid_program() {
//         let result = C1Parser::parse("  bool  ");
//         println!("{:?}", result);
//         assert!(result.is_err());
//
//         let result = C1Parser::parse("x = 0;");
//         println!("{:?}", result);
//         assert!(result.is_err());
//
//         let result = C1Parser::parse("// A valid comment\nInvalid line.");
//         println!("{:?}", result);
//         assert!(result.is_err());
//     }
//
//     #[test]
//     fn valid_function() {
//         let result = C1Parser::parse("  void foo() {}  ");
//         assert!(result.is_ok());
//
//         let result = C1Parser::parse("int bar() {return 0;}");
//         assert!(result.is_ok());
//
//         let result = C1Parser::parse(
//             "float calc() {\n\
//         x = 1.0;
//         y = 2.2;
//         return x + y;
//         \n\
//         }",
//         );
//         assert!(result.is_ok());
//     }
//
//     #[test]
//     fn fail_invalid_function() {
//         let result = C1Parser::parse("  void foo()) {}  ");
//         println!("{:?}", result);
//         assert!(result.is_err());
//
//         let result = C1Parser::parse("const bar() {return 0;}");
//         println!("{:?}", result);
//         assert!(result.is_err());
//
//         let result = C1Parser::parse(
//             "int bar() {
//                                                           return 0;
//                                                      int foo() {}",
//         );
//         println!("{:?}", result);
//         assert!(result.is_err());
//
//         let result = C1Parser::parse(
//             "float calc(int invalid) {\n\
//         x = 1.0;
//         y = 2.2;
//         return x + y;
//         \n\
//         }",
//         );
//         println!("{:?}", result);
//         assert!(result.is_err());
//     }
//
//     #[test]
//     fn valid_function_call() {
//         assert!(call_method(C1Parser::function_call, "foo()").is_ok());
//         assert!(call_method(C1Parser::function_call, "foo( )").is_ok());
//         assert!(call_method(C1Parser::function_call, "bar23( )").is_ok());
//     }
//
//     #[test]
//     fn fail_invalid_function_call() {
//         assert!(call_method(C1Parser::function_call, "foo)").is_err());
//         assert!(call_method(C1Parser::function_call, "foo{ )").is_err());
//         assert!(call_method(C1Parser::function_call, "bar _foo( )").is_err());
//     }
//
//     #[test]
//     fn valid_statement_list() {
//         assert!(call_method(C1Parser::statement_list, "x = 4;").is_ok());
//         assert!(call_method(
//             C1Parser::statement_list,
//             "x = 4;\n\
//         y = 2.1;"
//         )
//         .is_ok());
//         assert!(call_method(
//             C1Parser::statement_list,
//             "x = 4;\n\
//         {\
//         foo();\n\
//         }"
//         )
//         .is_ok());
//         assert!(call_method(C1Parser::statement_list, "{x = 4;}\ny = 1;\nfoo;\n{}").is_ok());
//     }
//
//     #[test]
//     fn fail_invalid_statement_list() {
//         assert!(call_method(
//             C1Parser::statement_list,
//             "x = 4\n\
//         y = 2.1;"
//         )
//         .is_err());
//         assert!(call_method(
//             C1Parser::statement_list,
//             "x = 4;\n\
//         {\
//         foo();"
//         )
//         .is_err());
//         assert!(call_method(C1Parser::statement_list, "{x = 4;\ny = 1;\nfoo;\n{}").is_err());
//     }
//
//     #[test]
//     fn valid_if_statement() {
//         assert!(call_method(C1Parser::if_statement, "if(x == 1) {}").is_ok());
//         assert!(call_method(C1Parser::if_statement, "if(x == y) {}").is_ok());
//         assert!(call_method(C1Parser::if_statement, "if(z) {}").is_ok());
//         assert!(call_method(C1Parser::if_statement, "if(true) {}").is_ok());
//         assert!(call_method(C1Parser::if_statement, "if(false) {}").is_ok());
//     }
//
//     #[test]
//     fn fail_invalid_if_statement() {
//         assert!(call_method(C1Parser::if_statement, "if(x == ) {}").is_err());
//         assert!(call_method(C1Parser::if_statement, "if( == y) {}").is_err());
//         assert!(call_method(C1Parser::if_statement, "if(> z) {}").is_err());
//         assert!(call_method(C1Parser::if_statement, "if( {}").is_err());
//         assert!(call_method(C1Parser::if_statement, "if(false) }").is_err());
//     }
//
//     #[test]
//     fn valid_return_statement() {
//         assert!(call_method(C1Parser::return_statement, "return x").is_ok());
//         assert!(call_method(C1Parser::return_statement, "return 1").is_ok());
//         assert!(call_method(C1Parser::return_statement, "return").is_ok());
//     }
//
//     #[test]
//     fn fail_invalid_return_statement() {
//         assert!(call_method(C1Parser::return_statement, "1").is_err());
//     }
//
//     #[test]
//     fn valid_printf_statement() {
//         assert!(call_method(C1Parser::printf, " printf(a+b)").is_ok());
//         assert!(call_method(C1Parser::printf, "printf( 1)").is_ok());
//         assert!(call_method(C1Parser::printf, "printf(a - c)").is_ok());
//     }
//
//     #[test]
//     fn fail_invalid_printf_statement() {
//         assert!(call_method(C1Parser::printf, "printf( ").is_err());
//         assert!(call_method(C1Parser::printf, "printf(printf)").is_err());
//         assert!(call_method(C1Parser::printf, "Printf()").is_err());
//     }
//
//     #[test]
//     fn valid_return_type() {
//         assert!(call_method(C1Parser::return_type, "void").is_ok());
//         assert!(call_method(C1Parser::return_type, "bool").is_ok());
//         assert!(call_method(C1Parser::return_type, "int").is_ok());
//         assert!(call_method(C1Parser::return_type, "float").is_ok());
//     }
//
//     #[test]
//     fn valid_assignment() {
//         assert!(call_method(C1Parser::assignment, "x = y").is_ok());
//         assert!(call_method(C1Parser::assignment, "x =y").is_ok());
//         assert!(call_method(C1Parser::assignment, "1 + 2").is_ok());
//     }
//
//     #[test]
//     fn valid_stat_assignment() {
//         assert!(call_method(C1Parser::stat_assignment, "x = y").is_ok());
//         assert!(call_method(C1Parser::stat_assignment, "x =y").is_ok());
//         assert!(call_method(C1Parser::stat_assignment, "x =y + t").is_ok());
//     }
//
//     #[test]
//     fn valid_factor() {
//         assert!(call_method(C1Parser::factor, "4").is_ok());
//         assert!(call_method(C1Parser::factor, "1.2").is_ok());
//         assert!(call_method(C1Parser::factor, "true").is_ok());
//         assert!(call_method(C1Parser::factor, "foo()").is_ok());
//         assert!(call_method(C1Parser::factor, "x").is_ok());
//         assert!(call_method(C1Parser::factor, "(x + y)").is_ok());
//     }
//
//     #[test]
//     fn fail_invalid_factor() {
//         assert!(call_method(C1Parser::factor, "if").is_err());
//         assert!(call_method(C1Parser::factor, "(4").is_err());
//         assert!(call_method(C1Parser::factor, "bool").is_err());
//     }
//
//     #[test]
//     fn multiple_functions() {
//         assert!(call_method(
//             C1Parser::program,
//             "void main() { hello();}\nfloat bar() {return 1.0;}"
//         )
//         .is_ok());
//     }
// }
