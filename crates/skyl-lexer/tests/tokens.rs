#[cfg(test)]
mod tests {
    use std::{cell::RefCell, rc::Rc};

    use skyl_data::{KeywordKind, Literal, OperatorKind, PunctuationKind, Token, TokenKind};
    use skyl_driver::errors::CompilerErrorReporter;
    use skyl_lexer::Lexer;

    fn generate_error_reporter() -> Rc<RefCell<CompilerErrorReporter>> {
        return Rc::new(RefCell::new(CompilerErrorReporter::empty()));
    }

    #[test]
    fn should_generate_operator_tokens() {
        let source = "+ ++ - -- > >= < <= == != * / not or and".into();

        let mut lexer = Lexer::new(source);
        let reporter = generate_error_reporter();
        let generated_tokens: Vec<Token> = lexer.scan_tokens(reporter).into();

        let expected_tokens = vec![
            OperatorKind::Plus,
            OperatorKind::PostFixIncrement,
            OperatorKind::Minus,
            OperatorKind::PostFixDecrement,
            OperatorKind::Greater,
            OperatorKind::GreaterEqual,
            OperatorKind::Less,
            OperatorKind::LessEqual,
            OperatorKind::EqualEqual,
            OperatorKind::NotEqual,
            OperatorKind::Star,
            OperatorKind::Slash,
            OperatorKind::Not,
            OperatorKind::Or,
            OperatorKind::And,
        ];

        // Expected tokens has more 1 token that represents the EndOfFile
        assert_eq!(
            generated_tokens.len(),
            expected_tokens.len() + 1,
            "The generated tokens count is not equal expected tokens count."
        );

        for (i, t) in expected_tokens.iter().enumerate() {
            if let TokenKind::Operator(o) = generated_tokens[i].kind {
                assert_eq!(t, &o, "Expect '{:?}' but got '{:?}'", t, &o);
            }
        }
    }

    #[test]
    fn should_generate_punctuation_tokens() {
        let source = "# , . ; ( ) [ ] { }".into();

        let mut lexer = Lexer::new(source);
        let reporter = generate_error_reporter();
        let generated_tokens: Vec<Token> = lexer.scan_tokens(reporter).into();

        let expected_tokens = vec![
            PunctuationKind::Hash,
            PunctuationKind::Comma,
            PunctuationKind::Dot,
            PunctuationKind::SemiColon,
            PunctuationKind::LeftParen,
            PunctuationKind::RightParen,
            PunctuationKind::LeftBracket,
            PunctuationKind::RightBracket,
            PunctuationKind::LeftBrace,
            PunctuationKind::RightBrace,
        ];

        // Expected tokens has more 1 token that represents the EndOfFile
        assert_eq!(
            generated_tokens.len(),
            expected_tokens.len() + 1,
            "The generated tokens count is not equal expected tokens count."
        );

        for (i, t) in expected_tokens.iter().enumerate() {
            if let TokenKind::Punctuation(p) = generated_tokens[i].kind {
                assert_eq!(t, &p, "Expect '{:?}' but got '{:?}'", t, &p);
            }
        }
    }

    #[test]
    fn should_generate_keyword_tokens() {
        let source = "if else elif while return def import
        lambda try except finally global type or and let in with native builtin
        attribute internal"
            .into();

        let mut lexer = Lexer::new(source);
        let reporter = generate_error_reporter();
        let generated_tokens: Vec<Token> = lexer.scan_tokens(reporter).into();

        let expected_tokens = vec![
            KeywordKind::If,
            KeywordKind::Else,
            KeywordKind::Elif,
            KeywordKind::While,
            KeywordKind::Return,
            KeywordKind::Def,
            KeywordKind::Import,
            KeywordKind::Lambda,
            KeywordKind::Try,
            KeywordKind::Except,
            KeywordKind::Finally,
            KeywordKind::Global,
            KeywordKind::Type,
            KeywordKind::Or,
            KeywordKind::And,
            KeywordKind::Let,
            KeywordKind::In,
            KeywordKind::With,
            KeywordKind::Native,
            KeywordKind::Builtin,
            KeywordKind::Attribute,
            KeywordKind::Internal,
        ];

        // Expected tokens has more 1 token that represents the EndOfFile
        assert_eq!(
            generated_tokens.len(),
            expected_tokens.len() + 1,
            "The generated tokens count is not equal expected tokens count."
        );

        for (i, t) in expected_tokens.iter().enumerate() {
            if let TokenKind::Keyword(k) = generated_tokens[i].kind {
                assert_eq!(t, &k, "Expect '{:?}' but got '{:?}'", t, &k);
            }
        }
    }

    #[test]
    fn should_generate_float_literal_tokens() {
        let source = "100.0 100.0. .10.0 .10.0.".into();
        let mut lexer = Lexer::new(source);
        let reporter = generate_error_reporter();
        let generated_tokens: Vec<Token> = lexer.scan_tokens(reporter).into();

        let expected_tokens = vec![
            TokenKind::Literal(Literal::Float),
            TokenKind::Literal(Literal::Float),
            TokenKind::Punctuation(PunctuationKind::Dot),
            TokenKind::Punctuation(PunctuationKind::Dot),
            TokenKind::Literal(Literal::Float),
            TokenKind::Punctuation(PunctuationKind::Dot),
            TokenKind::Literal(Literal::Float),
            TokenKind::Punctuation(PunctuationKind::Dot),
            TokenKind::EndOfFile,
        ];

        assert_eq!(
            generated_tokens.len(),
            expected_tokens.len(),
            "The generated tokens count is not equal expected tokens count."
        );

        for (i, t) in expected_tokens.iter().enumerate() {
            assert_eq!(
                t, &generated_tokens[i].kind,
                "Expect {:?}' but got '{:?}'.",
                t, &generated_tokens[i].kind
            );
        }
    }

    #[test]
    fn should_generate_int_literal_tokens() {
        let source = "1000 1000. .100 .100.".into();
        let mut lexer = Lexer::new(source);
        let reporter = generate_error_reporter();
        let generated_tokens: Vec<Token> = lexer.scan_tokens(reporter).into();

        let expected_tokens = vec![
            TokenKind::Literal(Literal::Int),
            TokenKind::Literal(Literal::Int),
            TokenKind::Punctuation(PunctuationKind::Dot),
            TokenKind::Punctuation(PunctuationKind::Dot),
            TokenKind::Literal(Literal::Int),
            TokenKind::Punctuation(PunctuationKind::Dot),
            TokenKind::Literal(Literal::Int),
            TokenKind::Punctuation(PunctuationKind::Dot),
            TokenKind::EndOfFile,
        ];

        assert_eq!(
            generated_tokens.len(),
            expected_tokens.len(),
            "The generated tokens count is not equal expected tokens count."
        );

        for (i, t) in expected_tokens.iter().enumerate() {
            assert_eq!(
                t, &generated_tokens[i].kind,
                "Expect {:?}' but got '{:?}'.",
                t, &generated_tokens[i].kind
            );
        }
    }

    #[test]
    fn should_generate_bool_literal_tokens() {
        let source = "true false .true. .false. true. false. .true .false".into();
        let mut lexer = Lexer::new(source);
        let reporter = generate_error_reporter();
        let generated_tokens: Vec<Token> = lexer.scan_tokens(reporter).into();

        let expected_tokens = vec![
            TokenKind::Literal(Literal::Boolean),
            TokenKind::Literal(Literal::Boolean),
            TokenKind::Punctuation(PunctuationKind::Dot),
            TokenKind::Literal(Literal::Boolean),
            TokenKind::Punctuation(PunctuationKind::Dot),
            TokenKind::Punctuation(PunctuationKind::Dot),
            TokenKind::Literal(Literal::Boolean),
            TokenKind::Punctuation(PunctuationKind::Dot),
            TokenKind::Literal(Literal::Boolean),
            TokenKind::Punctuation(PunctuationKind::Dot),
            TokenKind::Literal(Literal::Boolean),
            TokenKind::Punctuation(PunctuationKind::Dot),
            TokenKind::Punctuation(PunctuationKind::Dot),
            TokenKind::Literal(Literal::Boolean),
            TokenKind::Punctuation(PunctuationKind::Dot),
            TokenKind::Literal(Literal::Boolean),
            TokenKind::EndOfFile,
        ];

        assert_eq!(
            generated_tokens.len(),
            expected_tokens.len(),
            "The generated tokens count is not equal expected tokens count."
        );

        for (i, t) in expected_tokens.iter().enumerate() {
            assert_eq!(
                t, &generated_tokens[i].kind,
                "Expect {:?}' but got '{:?}'.",
                t, &generated_tokens[i].kind
            );
        }
    }

    #[test]
    fn should_generate_str_literal_tokens() {
        let source = "'Hello World' 
        \"Hello World\" 
        'Hello\\nWorld' 
        \"Hello\\nWorld\""
            .into();

        let mut lexer = Lexer::new(source);
        let reporter = generate_error_reporter();
        let generated_tokens: Vec<Token> = lexer.scan_tokens(reporter).into();

        let expected_tokens = vec![
            TokenKind::Literal(Literal::String),
            TokenKind::Literal(Literal::String),
            TokenKind::Literal(Literal::String),
            TokenKind::Literal(Literal::String),
            TokenKind::EndOfFile,
        ];

        assert_eq!(
            generated_tokens.len(),
            expected_tokens.len(),
            "The generated tokens count is not equal expected tokens count."
        );

        let expected_lexemes = vec!["Hello World", "Hello World", "Hello\nWorld", "Hello\nWorld"];

        for (i, t) in expected_tokens.iter().enumerate() {
            assert_eq!(
                t, &generated_tokens[i].kind,
                "Expect {:?}' but got '{:?}'.",
                t, &generated_tokens[i].kind
            );
        }

        for (i, l) in expected_lexemes.iter().enumerate() {
            assert_eq!(
                l, &generated_tokens[i].lexeme,
                "Expect {:?}' but got '{:?}'.",
                l, &generated_tokens[i].lexeme
            );
        }
    }

    #[test]
    fn should_ignore_comment_lines() {
        let source = "
            // Comment
            // Comment
            // Comment
            let x = 0;
        "
        .into();

        let mut lexer = Lexer::new(source);
        let reporter = generate_error_reporter();
        let generated_tokens: Vec<Token> = lexer.scan_tokens(reporter).into();

        let expected_tokens = vec![
            TokenKind::Keyword(KeywordKind::Let),
            TokenKind::Identifier,
            TokenKind::Operator(OperatorKind::Equal),
            TokenKind::Literal(Literal::Int),
            TokenKind::Punctuation(PunctuationKind::SemiColon),
            TokenKind::EndOfFile,
        ];

        assert_eq!(
            generated_tokens.len(),
            expected_tokens.len(),
            "The generated tokens count is not equal expected tokens count."
        );

        for (i, t) in expected_tokens.iter().enumerate() {
            assert_eq!(
                t, &generated_tokens[i].kind,
                "Expect {:?}' but got '{:?}'.",
                t, &generated_tokens[i].kind
            );
        }
    }

    #[test]
    fn should_report_invalid_character_error() {
        let source = "
            let x = 0; @ $
        "
        .into();

        let mut lexer = Lexer::new(source);
        let reporter = generate_error_reporter();
        let generated_tokens: Vec<Token> = lexer.scan_tokens(Rc::clone(&reporter)).into();

        let expected_tokens = vec![
            TokenKind::Keyword(KeywordKind::Let),
            TokenKind::Identifier,
            TokenKind::Operator(OperatorKind::Equal),
            TokenKind::Literal(Literal::Int),
            TokenKind::Punctuation(PunctuationKind::SemiColon),
            TokenKind::EndOfFile,
        ];

        assert_eq!(
            generated_tokens.len(),
            expected_tokens.len(),
            "The generated tokens count is not equal expected tokens count."
        );

        for (i, t) in expected_tokens.iter().enumerate() {
            assert_eq!(
                t, &generated_tokens[i].kind,
                "Expect {:?}' but got '{:?}'.",
                t, &generated_tokens[i].kind
            );
        }

        assert_eq!(
            reporter.borrow().has_errors(),
            true,
            "Expect reporter with errors."
        );

        let error_msgs = vec!["Invalid character", "Invalid character"];

        assert_eq!(
            error_msgs.len(),
            reporter.borrow().get_errors().len(),
            "Reporter has different error count than expected count."
        );

        let binding = reporter.borrow();
        let errors = binding.get_errors();

        for (i, error) in errors.iter().enumerate() {
            unimplemented!();
        }
    }
}
