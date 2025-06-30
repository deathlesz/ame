#![no_main]

use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &str| {
    // calling `.count()` here to consume the iterator
    ame_lexer::tokenize(data).count();
});
