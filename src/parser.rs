use crate::lexer::{C1Lexer, C1Token};
use crate::ParseResult;
use std::ops::{Deref, DerefMut};

pub struct C1Parser<'a>(C1Lexer<'a>);

type ParseResult<T> = Result<T, String>;

impl<'a> C1Parser<'a> {
     pub fn new(lexer: C1Lexer<'a>) -> Self {
        C1Parser(lexer)
    }
    
    pub fn parse(&mut self) -> ParseResult<()> {
        while let Some(token) = self.current_token() {
            match token {
                C1Token::KwPrintf => self.parse_printf_statement()?,
                _ => {
                    return Err(format!(
                        "Unexpected token {:?} at position {}",
                        token,
                        token.position()
                    ));
                }
            }
        }
        Ok(())
    }

    fn initialize_parser(text: &str) -> C1Parser {
        C1Parser(C1Lexer::new(text))
    }

    fn type_(&mut self) -> ParseResult {
        match self.current_token() {
            Some(token) => match token.token_type {
                C1Token::KwBoolean
                | C1Token::KwFloat
                | C1Token::KwInt
                | C1Token::KwVoid => {
                    self.eat();
                    Ok(())
                }
                _ => Err(format!("Expected type, found {:?}", token)),
            },
            None => Err("Unexpected end of input".to_string()),
        }
    }

    /// Check whether the current token is equal to the given token. If yes, consume it, otherwise
    /// return an error with the given error message
    
    fn expect_token(&mut self, expected_token: C1Token) -> ParseResult<()> {
        let token = self.next_token()?;
        if token == expected_token {
            Ok(())
        } else {
            Err(format!(
                "Expected {:?}, found {:?}",
                expected_token, token
            ))
        }
    }

    fn next_token(&mut self) -> ParseResult {
        let next_token = self.tokens.next();
        Ok(next_token)
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
            if token == &C1Token::Plus || token == &C1Token::Minus || token == &C1Token::Or {
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
            if token == &C1Token::Asterisk || token == &C1Token::Slash || token == &C1Token::And {
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
            self.function_definition()?;
        }
        self.expect_token(&C1Token::EOF)?;
        Ok(())
    }

    fn function_definition(&mut self) -> ParseResult {
        self.type_()?;
        self.expect_token(&C1Token::Identifier)?;
        self.expect_token(&C1Token::LeftParenthesis)?;
        self.expect_token(&C1Token::RightParenthesis)?;
        self.expect_token(&C1Token::LeftBrace)?;
        self.statement_list()?;
        self.expect_token(&C1Token::RightBrace)?;
        Ok(())
    }

    fn statement_assignment(&mut self) -> ParseResult {
        self.expect_token(&C1Token::Identifier)?;
        self.expect_token(&C1Token::Assign)?;
        self.assignment()?;
        Ok(())
    }

    fn function_call(&mut self) -> ParseResult {
        self.expect_token(&C1Token::Identifier)?;
        self.expect_token(&C1Token::LeftParenthesis)?;
        self.expect_token(&C1Token::RightParenthesis)?;
        Ok(())
    }

    fn block(&mut self) -> ParseResult {
        self.expect_token(&C1Token::LeftBrace)?;
        self.statement_list()?;
        self.expect_token(&C1Token::RightBrace)?;
        Ok(())
    }

    fn statement_list(&mut self) -> ParseResult {
        self.statement()?;
        if self.current_token().is_some() {
            self.statement_list()?;
        }
        Ok(())
    }

    fn statement(&mut self) -> ParseResult {
        if self.current_matches(&C1Token::KwIf) {
            self.if_statement()?;
        } else if self.current_matches(&C1Token::KwReturn) {
            self.return_statement()?;
        } else if self.current_matches(&C1Token::KwPrintf) {
            self.printf()?;
        } else if self.current_matches(&C1Token::Identifier) {
            if self.next_matches(&C1Token::Assign) {
                self.statement_assignment()?;
            } else {
                self.function_call()?;
            }
        } else if self.current_matches(&C1Token::LeftBrace) {
            self.block()?;
        } else {
            return Err(format!("Unexpected token: {:?}", self.current_token()));
        }
        self.expect_token(&C1Token::Semicolon)?;
        Ok(())
    }

    fn if_statement(&mut self) -> ParseResult {
        self.expect_token(&C1Token::KwIf)?;
        self.expect_token(&C1Token::LeftParenthesis)?;
        self.assignment()?;
        self.expect_token(&C1Token::RightParenthesis)?;
        self.block()?;
        Ok(())
    }

    fn return_statement(&mut self) -> ParseResult {
        self.expect_token(&C1Token::KwReturn)?;
        if !self.current_matches(&C1Token::Semicolon) {
            self.assignment()?;
        }
        Ok(())
    }
    
    fn parse_printf_statement(&mut self) -> ParseResult<()> {
        self.expect_token(C1Token::KwPrintf)?;
        self.expect_token(C1Token::LeftParenthesis)?;
        self.assignment()?;
        self.expect_token(C1Token::RightParenthesis)?;
        Ok(())
    }

    fn printf(&mut self) -> ParseResult {
        self.expect_token(&C1Token::KwPrintf)?;
        self.expect_token(&C1Token::LeftParenthesis)?;
        self.assignment()?;
        self.expect_token(&C1Token::RightParenthesis)?;
        Ok(())
    }

    // Helper methods

    /// For each token in the given slice, check whether the token is equal to the current token,
    /// consume the current token, and check the next token in the slice against the next token
    /// provided by the lexer.
    fn expect_tokens(&mut self, tokens: &[C1Token]) -> ParseResult {
        for token in tokens {
            self.expect_token(token)?;
        }
        Ok(())
    }

    /// Check whether the given token matches the current token
    fn current_matches(&self, token: &C1Token) -> bool {
        match self.current_token() {
            None => false,
            Some(current) => current == token,
        }
    }
    
    /// Check whether the given token matches the next token
    fn next_matches(&self, token: &C1Token) -> bool {
        match self.peek_token() {
            None => false,
            Some(next) => next == token,
        }
    }

    /// Check whether any of the tokens matches the current token.
    fn any_match_current(&self, tokens: &[C1Token]) -> bool {
        tokens.iter().any(|token| self.current_matches(token))
    }

    /// Check whether any of the tokens matches the current token, then consume it
    fn any_match_and_eat(&mut self, tokens: &[C1Token], error_message: &str) -> ParseResult {
        if tokens.iter().any(|token| self.check_and_eat_token(token, "").is_ok()) {
            Ok(())
        } else {
            Err(String::from(error_message))
        }
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

    /// Consume the current token and move to the next token
    fn eat(&mut self) {
        self.0.next();
    }

    /// Get the current token
    fn current_token(&self) -> Option<&C1Token> {
        self.0.current_token()
    }

    /// Get the next token without consuming it
    fn peek_token(&self) -> Option<&C1Token> {
        self.tokens.peek_token()
    }
    
    fn next_token(&mut self) -> ParseResult<C1Token> {
        self.0.next().ok_or("Unexpected end of input".to_string())
    }

    fn peek_token(&self) -> Option<&C1Token> {
        self.0.peek_token()
    }
}

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
