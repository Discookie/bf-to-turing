
use std::cmp::Ordering;
use std::env;
use std::error::Error;
use std::fmt;
use std::io::{self, Read};
use std::num::Wrapping;

use getopts::Options;
use nanoid::nanoid;

mod bfir;

use bfir::AstNode;

#[derive(PartialEq, Eq)]
pub enum MemoryStep {
    Left,
    Right,
    None
}

impl fmt::Display for MemoryStep {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            MemoryStep::Left => '<',
            MemoryStep::Right => '>',
            MemoryStep::None => '-',
        })
    }
}

pub struct TuringInstr {
    pub current_state: String,
    pub read_symbol: Wrapping<i8>,

    pub new_state: String,
    pub write_symbol: Wrapping<i8>,
    pub memory_step: MemoryStep,
    pub is_write: bool
}

const DISPLAY_CHARACTERS: &'static str =
//  0               1
//  0123456789ABCDEF0123456789ABCDEF
   "_¡¢£¤¥¦§¨©ª«¬®¯°±²³´µ¶·¸¹º»¼½¾¿À\
    Á!Â#$%&'()*+Ã-./0123456789:;<=>?\
    @ABCDEFGHIJKLMNOPQRSTUVWXYZ[Ä]^Å\
    `abcdefghijklmnopqrstuvwxyz{|}~Æ\
    ÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæ\
    çèéêëìíîïðñòóôõö÷øùúûüýþÿĀāĂăĄąĆ\
    ćĈĉĊċČčĎďĐđĒēĔĕĖėĘęĚěĜĝĞğĠġĢģĤĥĦ\
    ħĨĩĪīĬĭĮįİıĲĳĴĵĶķĸĹĺĻļĽľĿŀŁłŃńŅņ";

impl fmt::Display for TuringInstr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let convert_to_string = |symbol: Wrapping<i8>| {
            DISPLAY_CHARACTERS.chars().nth((symbol.0 as u8) as usize).unwrap()
        };

        let read_symbol = convert_to_string(self.read_symbol);
        let write_symbol = convert_to_string(self.write_symbol);

        let new_state = if self.new_state.is_empty() {
            "bad"
        } else {
            self.new_state.as_str()
        };

        writeln!(f, "{}, {}", self.current_state, read_symbol)?;
        writeln!(f, "{}, {}, {}", new_state, write_symbol, self.memory_step)?;

        Ok(())
    }
}

impl TuringInstr {
    fn as_two_tape_str(&self) -> String {
        let convert_to_string = |symbol: Wrapping<i8>| {
            DISPLAY_CHARACTERS.chars().nth((symbol.0 as u8) as usize).unwrap()
        };

        let read_symbol = convert_to_string(self.read_symbol);
        let write_symbol = convert_to_string(self.write_symbol);

        let new_state = if self.new_state.is_empty() {
            "bad"
        } else {
            self.new_state.as_str()
        };

        format!("{}, {}, _\n\
                 {}, {}, {}, {}, {}\n",
                self.current_state, read_symbol,
                new_state, write_symbol, if self.is_write { write_symbol } else { '_' }, 
                self.memory_step, if self.is_write { ">" } else { "-" })
    }
}

trait TuringSerialize {
    fn serialize_to_turing(&self) -> Vec<Vec<TuringInstr>>;
}

impl TuringSerialize for AstNode {
    /// Take care to always set the new_state to the next instruction, whenever it's empty
    fn serialize_to_turing(&self) -> Vec<Vec<TuringInstr>> {
        match self {
            AstNode::Increment { amount, offset, position } => {
                if *offset == 0 {
                    let position = position.expect("Cannot translate programs without position");
                    let state = format!("inc_{}_{}", position.start, nanoid!(6));
                    vec!(
                        (-128..=127).map(|read_value| {
                            let read_value = Wrapping(read_value);

                            TuringInstr {
                                current_state: state.clone(),
                                read_symbol: read_value,

                                new_state: String::new(), // intentional
                                write_symbol: read_value + amount,
                                memory_step: MemoryStep::None,

                                is_write: false
                            }
                        }).collect()
                    )
                } else {
                    let position = position.expect("Cannot translate programs without position");
                    vec![
                        AstNode::PointerIncrement{
                            amount: *offset,
                            position: Some(position)
                        },
                        AstNode::Increment{
                            amount: *amount,
                            offset: 0,
                            position: Some(position)
                        },
                        AstNode::PointerIncrement{
                            amount: -*offset,
                            position: Some(position)
                        },
                    ].serialize_to_turing()
                }
            },
            AstNode::PointerIncrement { amount, position } => {
                let position = position.expect("Cannot translate programs without position");
                let state = format!("ptr_{}_{}", position.start, nanoid!(6));
                vec!(
                    (-128..=127).map(|read_value| {
                        let mut instr_vec = Vec::new();
                        let read_value = Wrapping(read_value);
                    
                        for pos in 0..amount.abs() as usize-1 {
                            instr_vec.push(TuringInstr {
                                current_state: format!("{}_{}", state, pos),
                                read_symbol: read_value,
                                
                                new_state: format!("{}_{}", state, pos+1), // step the loop
                                write_symbol: read_value,
                                memory_step: match amount.cmp(&0) {
                                    Ordering::Less => MemoryStep::Left,
                                    Ordering::Equal => MemoryStep::None,
                                    Ordering::Greater => MemoryStep::Right,
                                },

                                is_write: false
                            });
                        }

                        instr_vec.push(TuringInstr {
                            current_state: format!("{}_{}", state, amount.abs()-1),
                            read_symbol: read_value,
                            
                            new_state: String::new(), // intentional
                            write_symbol: read_value,
                            memory_step: match amount.cmp(&0) {
                                Ordering::Less => MemoryStep::Left,
                                Ordering::Equal => MemoryStep::None,
                                Ordering::Greater => MemoryStep::Right,
                            },

                            is_write: false
                        });
                    
                        instr_vec
                    }).flatten().collect()
                )
            },
            AstNode::Read{..} => {
                panic!("Turing machines don't support read instructions");
            },
            AstNode::Write{position} => {
                let position = position.expect("Cannot translate programs without position");
                let state = format!("write_{}_{}", position.start, nanoid!(6));

                // Common ASCII characters are printed to the second tape, if TWO_TAPE is enabled
                let mut instrs: Vec<TuringInstr> = 
                    (33..=126).map(|read_value| {
                        let read_value = Wrapping(read_value);

                        TuringInstr {
                            current_state: state.clone(),
                            read_symbol: read_value,

                            new_state: String::new(), // intentional
                            write_symbol: read_value,
                            memory_step: MemoryStep::None,

                            is_write: true
                        }
                    }).collect();
                
                // Writing a different state is equivalent to an exit code
                // Exit code zero is the accepting state.
                let push_to_front = TuringInstr{
                    current_state: state.clone(),
                    read_symbol: Wrapping(0),
                    
                    new_state: String::from("end"),
                    write_symbol: Wrapping(0),
                    memory_step: MemoryStep::None,

                    is_write: false
                };

                instrs.insert(0, push_to_front);

                vec!(instrs)
            },
            AstNode::Loop { body, position } => {
                let position = position.expect("Cannot translate programs without position");

                let mut body_instr: Vec<Vec<TuringInstr>> = body.serialize_to_turing();

                if let Some(first_state) = body_instr.first().map(|instr| &instr.first().unwrap().current_state) {
                    let loop_start = format!("loop_{}_{}", position.start, nanoid!(6));
                    let loop_end = format!("loop_end_{}_{}", position.start, nanoid!(6));

                    let push_to_front = (-128..=127).map(|read_value| {
                        let read_value = Wrapping(read_value);

                        TuringInstr {
                            current_state: loop_start.clone(),
                            read_symbol: read_value,

                            new_state: if read_value.0 != 0 { first_state.clone() } else { loop_end.clone() },
                            write_symbol: read_value,
                            memory_step: MemoryStep::None,

                            is_write: false
                        }
                    }).collect();

                    body_instr.insert(0, push_to_front);

                    let push_to_back: Vec<TuringInstr> = (-128..=127).map(|read_value| {
                        let read_value = Wrapping(read_value);

                        // jump back on nonzero
                        TuringInstr {
                            current_state: loop_end.clone(),
                            read_symbol: read_value,

                            new_state: if read_value.0 != 0 { loop_start.clone() } else { String::new() },
                            write_symbol: read_value,
                            memory_step: MemoryStep::None,

                            is_write: false
                        }
                    }).collect();

                    for instr_to_change in body_instr.last_mut().unwrap() {
                        if instr_to_change.new_state.is_empty() {
                            instr_to_change.new_state = loop_end.clone();
                        }
                    }

                    body_instr.push(push_to_back);
                }

                body_instr
            },
            AstNode::MultiplyMove{changes, position} => {
                let position = position.expect("Cannot translate programs without position");
                // We need to re-create the multiply-move loop since we cannot use this optimization
                let mut changes: Vec<(isize, Wrapping<i8>)> = 
                    changes
                        .clone()
                        .into_iter()
                        .collect();
                changes.sort_by(|(l, _), (r, _)| l.cmp(r));
                
                let mut body_instr = Vec::new();

                for (offset, amount) in changes {
                    body_instr.push(AstNode::Increment{
                        amount,
                        offset,
                        position: Some(position)
                    });
                }
                
                body_instr.push(AstNode::Increment{
                    amount: Wrapping(-1),
                    offset: 0,
                    position: Some(position)
                });

                AstNode::Loop {
                    body: body_instr,
                    position: Some(position)
                }.serialize_to_turing()
            },
            AstNode::Set{amount, offset, position} => {
                if *offset == 0 {
                    let position = position.expect("Cannot translate programs without position");
                    let state = format!("set_{}_{}", position.start, nanoid!(6));
                    vec!(
                        (-128..=127).map(|read_value| {
                            let read_value = Wrapping(read_value);

                            TuringInstr {
                                current_state: state.clone(),
                                read_symbol: read_value,

                                new_state: String::new(), // intentional
                                write_symbol: *amount,
                                memory_step: MemoryStep::None,

                                is_write: false
                            }
                        }).collect()
                    )
                } else {
                    let position = position.expect("Cannot translate programs without position");
                    vec![
                        AstNode::PointerIncrement{
                            amount: *offset,
                            position: Some(position)
                        },
                        AstNode::Set{
                            amount: *amount,
                            offset: 0,
                            position: Some(position)
                        },
                        AstNode::PointerIncrement{
                            amount: -*offset,
                            position: Some(position)
                        },
                    ].serialize_to_turing()
                }
            }
        }
    }
}

impl TuringSerialize for Vec<AstNode> {
    fn serialize_to_turing(&self) -> Vec<Vec<TuringInstr>> {
        let mut turing_instrs: Vec<Vec<TuringInstr>> = 
            self.iter()
                .flat_map(|instr| instr.serialize_to_turing())
                .filter(|set| !set.is_empty())
                .collect();
        
        // correct the next state for empty ones
        for i in 0..turing_instrs.len()-1 {
            let next_state = turing_instrs[i+1].first().unwrap().current_state.clone();
            if next_state == turing_instrs[i].first().unwrap().current_state {
                eprintln!("old state: {}, new state: {}", turing_instrs[i+1].first().unwrap(), turing_instrs[i].first().unwrap());
                panic!("state duplication");
            }
            for instr in turing_instrs[i].iter_mut() {
                if instr.new_state.is_empty() {
                    instr.new_state = next_state.clone();
                }
            }
        }

        turing_instrs
    }
}

fn print_usage(bin_name: &str, opts: Options) {
    let brief = format!("Usage: {} [options]  -- pass BFIR JSON via stdin!", bin_name);
    print!("{}", opts.usage(&brief));
}

fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<_> = env::args().collect();
    let mut opts = Options::new();

    opts.optflag("h", "help", "print usage");
    opts.optflag("", "two-tape", "enables minimal two-tape mode");
    
    let matches = match opts.parse(&args[1..]) {
        Ok(m) => m,
        Err(x) => {
            eprintln!("{}", x.to_string());
            print_usage(&args[0], opts);
            std::process::exit(1);
        }
    };

    if matches.opt_present("h") {
        print_usage(&args[0], opts);
        return Ok(());
    }

    let read_str = {
        let mut stdin = io::stdin();
        let mut read_str = String::new();
        stdin.read_to_string(&mut read_str)?;
        read_str
    };

    if read_str.is_empty() {
        return Err(Box::from( "no input specified".to_string() ));
    }

    let instrs = serde_json::from_str::<Vec<AstNode>>(read_str.as_str())?;
    let serialized = instrs.serialize_to_turing();

    if serialized.is_empty() {
        return Ok(());
    }

    let first_name = 
        serialized.first().unwrap()
        .first()
        .expect("Invalid code generated")
        .current_state.clone();

    println!("// Generated from Brainfuck via bfc and bf-to-turing");
    println!();
    println!("name: bf-compiled-output");
    println!("init: {}", first_name);
    println!("accept: end");
    println!();
    println!("// For convenience, here is the used byte values");
    println!("//      0               1");
    println!("//      0123456789ABCDEF0123456789ABCDEF");
    for i in (0u8..=255).step_by(0x20) {
        println!(
            "// 0x{:02x} {}", 
            i, 
            DISPLAY_CHARACTERS.chars()
                .skip(i as usize)
                .take(32)
                .collect::<String>()
        );
    }
    println!();

    let is_two_tape = matches.opt_present("two-tape");
    for instr in serialized.iter().flatten() {
        if is_two_tape {
            println!("{}", instr.as_two_tape_str());
        } else {
            println!("{}", instr);
        }
    }

    Ok(())
}

#[test]
fn full_input_test() {
    let read_str = r#"[{"Increment":{"amount":-1,"offset":-8,"position":{"start":884,"end":884}}},{"Increment":{"amount":-1,"offset":-6,"position":{"start":880,"end":880}}},{"Increment":{"amount":-1,"offset":-4,"position":{"start":876,"end":876}}},{"Increment":{"amount":-1,"offset":-2,"position":{"start":872,"end":872}}},{"PointerIncrement":{"amount":1,"position":{"start":988,"end":990}}},{"Loop":{"body":[{"Set":{"amount":0,"offset":0,"position":{"start":1209,"end":1211}}},{"PointerIncrement":{"amount":1,"position":{"start":1250,"end":1250}}},{"Loop":{"body":[{"PointerIncrement":{"amount":1,"position":{"start":1253,"end":1253}}}],"position":{"start":1252,"end":1254}}},{"Set":{"amount":1,"offset":0,"position":{"start":1308,"end":1308}}},{"Increment":{"amount":8,"offset":1,"position":{"start":1317,"end":1320}}},{"PointerIncrement":{"amount":1,"position":{"start":1310,"end":1310}}},{"MultiplyMove":{"changes":{"-1":12},"position":{"start":1348,"end":1398}}},{"PointerIncrement":{"amount":-2,"position":{"start":1407,"end":1408}}},{"Loop":{"body":[{"PointerIncrement":{"amount":-1,"position":{"start":1411,"end":1411}}}],"position":{"start":1410,"end":1412}}},{"PointerIncrement":{"amount":2,"position":{"start":1414,"end":1415}}}],"position":{"start":1121,"end":1453}}},{"Increment":{"amount":1,"offset":-1,"position":{"start":1514,"end":1514}}},{"Increment":{"amount":-1,"offset":1,"position":{"start":1462,"end":1462}}},{"PointerIncrement":{"amount":-1,"position":{"start":1464,"end":1465}}},{"Loop":{"body":[{"Increment":{"amount":1,"offset":-2,"position":{"start":1523,"end":1523}}},{"Increment":{"amount":-1,"offset":0,"position":{"start":1518,"end":1518}}},{"PointerIncrement":{"amount":-2,"position":{"start":1520,"end":1521}}}],"position":{"start":1516,"end":1525}}},{"Increment":{"amount":1,"offset":-8,"position":{"start":1618,"end":1618}}},{"Set":{"amount":-1,"offset":0,"position":{"start":1527,"end":1527}}},{"PointerIncrement":{"amount":-8,"position":{"start":1615,"end":1616}}},{"Loop":{"body":[{"Increment":{"amount":1,"offset":4,"position":{"start":1827,"end":1827}}},{"PointerIncrement":{"amount":4,"position":{"start":1824,"end":1825}}},{"Loop":{"body":[{"Increment":{"amount":-1,"offset":0,"position":{"start":1831,"end":1831}}},{"Increment":{"amount":1,"offset":2,"position":{"start":1836,"end":1836}}},{"PointerIncrement":{"amount":2,"position":{"start":1833,"end":1834}}}],"position":{"start":1829,"end":1838}}},{"Set":{"amount":-1,"offset":0,"position":{"start":1840,"end":1840}}},{"Increment":{"amount":1,"offset":2,"position":{"start":1870,"end":1870}}},{"PointerIncrement":{"amount":2,"position":{"start":1867,"end":1868}}},{"Loop":{"body":[{"Increment":{"amount":-1,"offset":0,"position":{"start":1874,"end":1874}}},{"Increment":{"amount":1,"offset":2,"position":{"start":1879,"end":1879}}},{"PointerIncrement":{"amount":2,"position":{"start":1876,"end":1877}}}],"position":{"start":1872,"end":1881}}},{"Set":{"amount":-1,"offset":0,"position":{"start":1883,"end":1883}}},{"Increment":{"amount":1,"offset":2,"position":{"start":1913,"end":1913}}},{"PointerIncrement":{"amount":2,"position":{"start":1910,"end":1911}}},{"Loop":{"body":[{"Increment":{"amount":-1,"offset":0,"position":{"start":1917,"end":1917}}},{"Increment":{"amount":1,"offset":2,"position":{"start":1922,"end":1922}}},{"PointerIncrement":{"amount":2,"position":{"start":1919,"end":1920}}}],"position":{"start":1915,"end":1924}}},{"Set":{"amount":-1,"offset":0,"position":{"start":1926,"end":1926}}},{"Increment":{"amount":1,"offset":2,"position":{"start":1958,"end":1958}}},{"PointerIncrement":{"amount":2,"position":{"start":1955,"end":1956}}},{"Loop":{"body":[{"Increment":{"amount":-1,"offset":0,"position":{"start":1962,"end":1962}}},{"Set":{"amount":0,"offset":1,"position":{"start":2024,"end":2026}}},{"Increment":{"amount":1,"offset":2,"position":{"start":2034,"end":2034}}},{"PointerIncrement":{"amount":2,"position":{"start":2028,"end":2028}}}],"position":{"start":1960,"end":2036}}},{"Increment":{"amount":1,"offset":-2,"position":{"start":2089,"end":2089}}},{"Set":{"amount":-1,"offset":0,"position":{"start":2038,"end":2038}}},{"Set":{"amount":0,"offset":1,"position":{"start":2042,"end":2044}}},{"PointerIncrement":{"amount":-2,"position":{"start":2086,"end":2087}}},{"Loop":{"body":[{"Increment":{"amount":1,"offset":-2,"position":{"start":2098,"end":2098}}},{"Increment":{"amount":-1,"offset":0,"position":{"start":2093,"end":2093}}},{"PointerIncrement":{"amount":-2,"position":{"start":2095,"end":2096}}}],"position":{"start":2091,"end":2100}}},{"Increment":{"amount":1,"offset":-2,"position":{"start":2132,"end":2132}}},{"Set":{"amount":-1,"offset":0,"position":{"start":2102,"end":2102}}},{"PointerIncrement":{"amount":-2,"position":{"start":2129,"end":2130}}},{"Loop":{"body":[{"Increment":{"amount":1,"offset":-2,"position":{"start":2141,"end":2141}}},{"Increment":{"amount":-1,"offset":0,"position":{"start":2136,"end":2136}}},{"PointerIncrement":{"amount":-2,"position":{"start":2138,"end":2139}}}],"position":{"start":2134,"end":2143}}},{"Increment":{"amount":1,"offset":-2,"position":{"start":2175,"end":2175}}},{"Set":{"amount":-1,"offset":0,"position":{"start":2145,"end":2145}}},{"PointerIncrement":{"amount":-2,"position":{"start":2172,"end":2173}}},{"Loop":{"body":[{"Increment":{"amount":1,"offset":-2,"position":{"start":2184,"end":2184}}},{"Increment":{"amount":-1,"offset":0,"position":{"start":2179,"end":2179}}},{"PointerIncrement":{"amount":-2,"position":{"start":2181,"end":2182}}}],"position":{"start":2177,"end":2186}}},{"Increment":{"amount":1,"offset":-2,"position":{"start":2364,"end":2364}}},{"Set":{"amount":-1,"offset":0,"position":{"start":2188,"end":2188}}},{"PointerIncrement":{"amount":-2,"position":{"start":2213,"end":2214}}},{"Loop":{"body":[{"Increment":{"amount":-1,"offset":0,"position":{"start":2370,"end":2370}}},{"Increment":{"amount":1,"offset":2,"position":{"start":2466,"end":2466}}},{"PointerIncrement":{"amount":2,"position":{"start":2463,"end":2464}}},{"Loop":{"body":[{"Increment":{"amount":-1,"offset":0,"position":{"start":2470,"end":2470}}},{"Increment":{"amount":1,"offset":2,"position":{"start":2475,"end":2475}}},{"PointerIncrement":{"amount":2,"position":{"start":2472,"end":2473}}}],"position":{"start":2468,"end":2477}}},{"Set":{"amount":-1,"offset":0,"position":{"start":2479,"end":2479}}},{"Increment":{"amount":1,"offset":2,"position":{"start":2513,"end":2513}}},{"PointerIncrement":{"amount":2,"position":{"start":2510,"end":2511}}},{"Loop":{"body":[{"Set":{"amount":0,"offset":0,"position":{"start":2566,"end":2568}}},{"Increment":{"amount":1,"offset":2,"position":{"start":2611,"end":2611}}},{"PointerIncrement":{"amount":2,"position":{"start":2570,"end":2571}}}],"position":{"start":2515,"end":2613}}},{"Increment":{"amount":1,"offset":-2,"position":{"start":2820,"end":2820}}},{"Set":{"amount":-1,"offset":0,"position":{"start":2615,"end":2615}}},{"PointerIncrement":{"amount":-2,"position":{"start":2669,"end":2670}}},{"Loop":{"body":[{"Increment":{"amount":-1,"offset":0,"position":{"start":2826,"end":2826}}},{"Increment":{"amount":1,"offset":2,"position":{"start":2922,"end":2922}}},{"PointerIncrement":{"amount":2,"position":{"start":2919,"end":2920}}},{"Loop":{"body":[{"Increment":{"amount":-1,"offset":0,"position":{"start":2926,"end":2926}}},{"Increment":{"amount":1,"offset":2,"position":{"start":2931,"end":2931}}},{"PointerIncrement":{"amount":2,"position":{"start":2928,"end":2929}}}],"position":{"start":2924,"end":2933}}},{"Set":{"amount":-1,"offset":0,"position":{"start":2935,"end":2935}}},{"Increment":{"amount":1,"offset":2,"position":{"start":2969,"end":2969}}},{"PointerIncrement":{"amount":2,"position":{"start":2966,"end":2967}}},{"Loop":{"body":[{"Set":{"amount":0,"offset":0,"position":{"start":3022,"end":3024}}},{"Increment":{"amount":1,"offset":2,"position":{"start":3067,"end":3067}}},{"PointerIncrement":{"amount":2,"position":{"start":3026,"end":3027}}}],"position":{"start":2971,"end":3069}}},{"Increment":{"amount":1,"offset":-2,"position":{"start":3276,"end":3276}}},{"Set":{"amount":-1,"offset":0,"position":{"start":3071,"end":3071}}},{"PointerIncrement":{"amount":-2,"position":{"start":3125,"end":3126}}},{"Loop":{"body":[{"Loop":{"body":[{"Increment":{"amount":-1,"offset":0,"position":{"start":3336,"end":3336}}},{"Increment":{"amount":1,"offset":2,"position":{"start":3341,"end":3341}}},{"PointerIncrement":{"amount":2,"position":{"start":3338,"end":3339}}}],"position":{"start":3334,"end":3343}}},{"Set":{"amount":-1,"offset":0,"position":{"start":3345,"end":3345}}},{"PointerIncrement":{"amount":3,"position":{"start":3462,"end":3464}}},{"Loop":{"body":[{"PointerIncrement":{"amount":2,"position":{"start":3517,"end":3518}}}],"position":{"start":3516,"end":3519}}},{"Set":{"amount":8,"offset":0,"position":{"start":3578,"end":3581}}},{"MultiplyMove":{"changes":{"-1":-12},"position":{"start":3592,"end":3713}}},{"Increment":{"amount":-1,"offset":-1,"position":{"start":3717,"end":3717}}},{"Increment":{"amount":1,"offset":0,"position":{"start":3732,"end":3732}}},{"PointerIncrement":{"amount":-1,"position":{"start":3863,"end":3863}}},{"Loop":{"body":[{"Set":{"amount":1,"offset":0,"position":{"start":4060,"end":4060}}},{"Loop":{"body":[{"Increment":{"amount":1,"offset":-2,"position":{"start":4069,"end":4069}}},{"Increment":{"amount":-1,"offset":0,"position":{"start":4064,"end":4064}}},{"PointerIncrement":{"amount":-2,"position":{"start":4066,"end":4067}}}],"position":{"start":4062,"end":4071}}},{"Increment":{"amount":1,"offset":-2,"position":{"start":4100,"end":4100}}},{"Set":{"amount":-1,"offset":0,"position":{"start":4073,"end":4073}}},{"PointerIncrement":{"amount":-2,"position":{"start":4097,"end":4098}}},{"Loop":{"body":[{"Increment":{"amount":1,"offset":-2,"position":{"start":4106,"end":4106}}},{"Increment":{"amount":-1,"offset":0,"position":{"start":4103,"end":4103}}},{"PointerIncrement":{"amount":-2,"position":{"start":4104,"end":4105}}}],"position":{"start":4101,"end":4108}}},{"Increment":{"amount":1,"offset":-2,"position":{"start":4137,"end":4137}}},{"Set":{"amount":-1,"offset":0,"position":{"start":4108,"end":4109}}},{"PointerIncrement":{"amount":-2,"position":{"start":4134,"end":4135}}},{"Loop":{"body":[{"Increment":{"amount":1,"offset":-2,"position":{"start":4143,"end":4143}}},{"Increment":{"amount":-1,"offset":0,"position":{"start":4140,"end":4140}}},{"PointerIncrement":{"amount":-2,"position":{"start":4141,"end":4142}}}],"position":{"start":4138,"end":4145}}},{"Increment":{"amount":1,"offset":-2,"position":{"start":4174,"end":4174}}},{"Set":{"amount":-1,"offset":0,"position":{"start":4145,"end":4146}}},{"PointerIncrement":{"amount":-2,"position":{"start":4171,"end":4172}}},{"Loop":{"body":[{"Increment":{"amount":1,"offset":-2,"position":{"start":4180,"end":4180}}},{"Increment":{"amount":-1,"offset":0,"position":{"start":4177,"end":4177}}},{"PointerIncrement":{"amount":-2,"position":{"start":4178,"end":4179}}}],"position":{"start":4175,"end":4182}}},{"Set":{"amount":-1,"offset":0,"position":{"start":4182,"end":4183}}},{"PointerIncrement":{"amount":-2,"position":{"start":4210,"end":4211}}},{"Loop":{"body":[{"Increment":{"amount":-1,"offset":0,"position":{"start":4214,"end":4214}}},{"Loop":{"body":[{"Increment":{"amount":1,"offset":0,"position":{"start":4217,"end":4217}}}],"position":{"start":4216,"end":4218}}}],"position":{"start":4213,"end":4219}}},{"Set":{"amount":-1,"offset":0,"position":{"start":4221,"end":4221}}},{"Increment":{"amount":1,"offset":4,"position":{"start":4275,"end":4275}}},{"PointerIncrement":{"amount":4,"position":{"start":4272,"end":4273}}},{"Loop":{"body":[{"Increment":{"amount":-1,"offset":0,"position":{"start":4278,"end":4278}}},{"Increment":{"amount":1,"offset":2,"position":{"start":4281,"end":4281}}},{"PointerIncrement":{"amount":2,"position":{"start":4279,"end":4280}}}],"position":{"start":4276,"end":4283}}},{"Set":{"amount":-1,"offset":0,"position":{"start":4283,"end":4284}}},{"Increment":{"amount":1,"offset":2,"position":{"start":4312,"end":4312}}},{"PointerIncrement":{"amount":2,"position":{"start":4309,"end":4310}}},{"Loop":{"body":[{"Increment":{"amount":-1,"offset":0,"position":{"start":4315,"end":4315}}},{"Increment":{"amount":1,"offset":2,"position":{"start":4318,"end":4318}}},{"PointerIncrement":{"amount":2,"position":{"start":4316,"end":4317}}}],"position":{"start":4313,"end":4320}}},{"Set":{"amount":-1,"offset":0,"position":{"start":4320,"end":4321}}},{"Increment":{"amount":1,"offset":2,"position":{"start":4349,"end":4349}}},{"PointerIncrement":{"amount":2,"position":{"start":4346,"end":4347}}},{"Loop":{"body":[{"Increment":{"amount":-1,"offset":0,"position":{"start":4352,"end":4352}}},{"Increment":{"amount":1,"offset":2,"position":{"start":4355,"end":4355}}},{"PointerIncrement":{"amount":2,"position":{"start":4353,"end":4354}}}],"position":{"start":4350,"end":4357}}},{"Set":{"amount":-1,"offset":0,"position":{"start":4357,"end":4358}}},{"Loop":{"body":[{"PointerIncrement":{"amount":2,"position":{"start":4390,"end":4391}}}],"position":{"start":4387,"end":4394}}},{"Increment":{"amount":-1,"offset":1,"position":{"start":4453,"end":4453}}}],"position":{"start":3865,"end":4499}}},{"PointerIncrement":{"amount":1,"position":{"start":4510,"end":4510}}},{"Loop":{"body":[{"Increment":{"amount":7,"offset":0,"position":{"start":4577,"end":4579}}},{"MultiplyMove":{"changes":{"-1":12},"position":{"start":4592,"end":4654}}},{"Increment":{"amount":2,"offset":-1,"position":{"start":4658,"end":4659}}}],"position":{"start":4512,"end":4730}}},{"Set":{"amount":1,"offset":0,"position":{"start":4811,"end":4811}}},{"PointerIncrement":{"amount":-1,"position":{"start":4872,"end":4872}}},{"Loop":{"body":[{"Increment":{"amount":-1,"offset":0,"position":{"start":4878,"end":4878}}},{"Increment":{"amount":1,"offset":2,"position":{"start":4883,"end":4883}}},{"PointerIncrement":{"amount":2,"position":{"start":4880,"end":4881}}}],"position":{"start":4876,"end":4885}}},{"Increment":{"amount":1,"offset":-2,"position":{"start":4957,"end":4957}}},{"Set":{"amount":-1,"offset":0,"position":{"start":4887,"end":4887}}},{"Set":{"amount":0,"offset":1,"position":{"start":4937,"end":4939}}},{"PointerIncrement":{"amount":-2,"position":{"start":4954,"end":4955}}},{"Loop":{"body":[{"Increment":{"amount":1,"offset":-2,"position":{"start":4966,"end":4966}}},{"Increment":{"amount":-1,"offset":0,"position":{"start":4961,"end":4961}}},{"PointerIncrement":{"amount":-2,"position":{"start":4963,"end":4964}}}],"position":{"start":4959,"end":4968}}},{"Set":{"amount":-1,"offset":0,"position":{"start":4970,"end":4970}}},{"PointerIncrement":{"amount":-2,"position":{"start":4998,"end":4999}}},{"Loop":{"body":[{"PointerIncrement":{"amount":-2,"position":{"start":5002,"end":5003}}}],"position":{"start":5001,"end":5004}}},{"Increment":{"amount":1,"offset":-2,"position":{"start":5082,"end":5082}}},{"Set":{"amount":1,"offset":0,"position":{"start":5006,"end":5006}}},{"PointerIncrement":{"amount":-2,"position":{"start":5008,"end":5009}}}],"position":{"start":3279,"end":5084}}},{"Set":{"amount":-1,"offset":0,"position":{"start":5086,"end":5086}}},{"PointerIncrement":{"amount":-2,"position":{"start":5161,"end":5162}}},{"Loop":{"body":[{"PointerIncrement":{"amount":-2,"position":{"start":5165,"end":5166}}}],"position":{"start":5164,"end":5167}}},{"Increment":{"amount":1,"offset":-2,"position":{"start":5247,"end":5247}}},{"Set":{"amount":1,"offset":0,"position":{"start":5169,"end":5169}}},{"PointerIncrement":{"amount":-2,"position":{"start":5171,"end":5172}}}],"position":{"start":2823,"end":5249}}},{"Set":{"amount":-1,"offset":0,"position":{"start":5251,"end":5251}}},{"PointerIncrement":{"amount":-2,"position":{"start":5326,"end":5327}}},{"Loop":{"body":[{"PointerIncrement":{"amount":-2,"position":{"start":5330,"end":5331}}}],"position":{"start":5329,"end":5332}}},{"Increment":{"amount":1,"offset":-2,"position":{"start":5412,"end":5412}}},{"Set":{"amount":1,"offset":0,"position":{"start":5334,"end":5334}}},{"PointerIncrement":{"amount":-2,"position":{"start":5336,"end":5337}}}],"position":{"start":2367,"end":5414}}},{"Set":{"amount":-1,"offset":0,"position":{"start":5416,"end":5416}}},{"Increment":{"amount":1,"offset":2,"position":{"start":5565,"end":5565}}},{"PointerIncrement":{"amount":2,"position":{"start":5562,"end":5563}}},{"Loop":{"body":[{"Increment":{"amount":-1,"offset":0,"position":{"start":5569,"end":5569}}},{"Increment":{"amount":1,"offset":2,"position":{"start":5574,"end":5574}}},{"PointerIncrement":{"amount":2,"position":{"start":5571,"end":5572}}}],"position":{"start":5567,"end":5576}}},{"Set":{"amount":-1,"offset":0,"position":{"start":5578,"end":5578}}},{"Increment":{"amount":1,"offset":2,"position":{"start":5610,"end":5610}}},{"PointerIncrement":{"amount":2,"position":{"start":5607,"end":5608}}},{"Loop":{"body":[{"Increment":{"amount":-1,"offset":0,"position":{"start":5614,"end":5614}}},{"Increment":{"amount":1,"offset":2,"position":{"start":5619,"end":5619}}},{"PointerIncrement":{"amount":2,"position":{"start":5616,"end":5617}}}],"position":{"start":5612,"end":5621}}},{"Set":{"amount":-1,"offset":0,"position":{"start":5623,"end":5623}}},{"Increment":{"amount":1,"offset":2,"position":{"start":5655,"end":5655}}},{"PointerIncrement":{"amount":2,"position":{"start":5652,"end":5653}}},{"Loop":{"body":[{"Increment":{"amount":-1,"offset":0,"position":{"start":5659,"end":5659}}},{"Increment":{"amount":1,"offset":2,"position":{"start":5664,"end":5664}}},{"PointerIncrement":{"amount":2,"position":{"start":5661,"end":5662}}}],"position":{"start":5657,"end":5666}}},{"Set":{"amount":-1,"offset":0,"position":{"start":5668,"end":5668}}},{"PointerIncrement":{"amount":3,"position":{"start":5697,"end":5699}}},{"Loop":{"body":[{"PointerIncrement":{"amount":2,"position":{"start":5703,"end":5704}}}],"position":{"start":5701,"end":5706}}},{"Set":{"amount":8,"offset":0,"position":{"start":5765,"end":5768}}},{"MultiplyMove":{"changes":{"-1":-12},"position":{"start":5834,"end":5867}}},{"Increment":{"amount":-1,"offset":-1,"position":{"start":5871,"end":5871}}},{"Increment":{"amount":1,"offset":0,"position":{"start":5880,"end":5880}}},{"PointerIncrement":{"amount":-1,"position":{"start":6011,"end":6011}}},{"Loop":{"body":[{"Set":{"amount":1,"offset":0,"position":{"start":6140,"end":6140}}},{"Loop":{"body":[{"Increment":{"amount":1,"offset":-2,"position":{"start":6149,"end":6149}}},{"Increment":{"amount":-1,"offset":0,"position":{"start":6144,"end":6144}}},{"PointerIncrement":{"amount":-2,"position":{"start":6146,"end":6147}}}],"position":{"start":6142,"end":6151}}},{"Increment":{"amount":1,"offset":-2,"position":{"start":6186,"end":6186}}},{"Set":{"amount":-1,"offset":0,"position":{"start":6153,"end":6153}}},{"PointerIncrement":{"amount":-2,"position":{"start":6183,"end":6184}}},{"Loop":{"body":[{"Increment":{"amount":1,"offset":-2,"position":{"start":6195,"end":6195}}},{"Increment":{"amount":-1,"offset":0,"position":{"start":6190,"end":6190}}},{"PointerIncrement":{"amount":-2,"position":{"start":6192,"end":6193}}}],"position":{"start":6188,"end":6197}}},{"Increment":{"amount":1,"offset":-2,"position":{"start":6229,"end":6229}}},{"Set":{"amount":-1,"offset":0,"position":{"start":6199,"end":6199}}},{"PointerIncrement":{"amount":-2,"position":{"start":6226,"end":6227}}},{"Loop":{"body":[{"Increment":{"amount":1,"offset":-2,"position":{"start":6238,"end":6238}}},{"Increment":{"amount":-1,"offset":0,"position":{"start":6233,"end":6233}}},{"PointerIncrement":{"amount":-2,"position":{"start":6235,"end":6236}}}],"position":{"start":6231,"end":6240}}},{"Increment":{"amount":1,"offset":-2,"position":{"start":6272,"end":6272}}},{"Set":{"amount":-1,"offset":0,"position":{"start":6242,"end":6242}}},{"PointerIncrement":{"amount":-2,"position":{"start":6269,"end":6270}}},{"Loop":{"body":[{"Increment":{"amount":1,"offset":-2,"position":{"start":6281,"end":6281}}},{"Increment":{"amount":-1,"offset":0,"position":{"start":6276,"end":6276}}},{"PointerIncrement":{"amount":-2,"position":{"start":6278,"end":6279}}}],"position":{"start":6274,"end":6283}}},{"Increment":{"amount":-1,"offset":-2,"position":{"start":6384,"end":6384}}},{"Set":{"amount":-1,"offset":0,"position":{"start":6285,"end":6285}}},{"PointerIncrement":{"amount":-2,"position":{"start":6314,"end":6315}}},{"Loop":{"body":[{"Loop":{"body":[{"Increment":{"amount":1,"offset":0,"position":{"start":6389,"end":6389}}}],"position":{"start":6388,"end":6390}}},{"Increment":{"amount":1,"offset":1,"position":{"start":6394,"end":6394}}}],"position":{"start":6386,"end":6398}}},{"PointerIncrement":{"amount":1,"position":{"start":6485,"end":6485}}},{"Loop":{"body":[{"Increment":{"amount":-1,"offset":-1,"position":{"start":6495,"end":6495}}},{"Set":{"amount":0,"offset":0,"position":{"start":6489,"end":6491}}}],"position":{"start":6487,"end":6499}}},{"Increment":{"amount":1,"offset":3,"position":{"start":6598,"end":6598}}},{"PointerIncrement":{"amount":3,"position":{"start":6595,"end":6596}}},{"Loop":{"body":[{"Increment":{"amount":-1,"offset":0,"position":{"start":6602,"end":6602}}},{"Increment":{"amount":1,"offset":2,"position":{"start":6607,"end":6607}}},{"PointerIncrement":{"amount":2,"position":{"start":6604,"end":6605}}}],"position":{"start":6600,"end":6609}}},{"Set":{"amount":-1,"offset":0,"position":{"start":6611,"end":6611}}},{"Increment":{"amount":1,"offset":2,"position":{"start":6641,"end":6641}}},{"PointerIncrement":{"amount":2,"position":{"start":6638,"end":6639}}},{"Loop":{"body":[{"Increment":{"amount":-1,"offset":0,"position":{"start":6645,"end":6645}}},{"Increment":{"amount":1,"offset":2,"position":{"start":6650,"end":6650}}},{"PointerIncrement":{"amount":2,"position":{"start":6647,"end":6648}}}],"position":{"start":6643,"end":6652}}},{"Set":{"amount":-1,"offset":0,"position":{"start":6654,"end":6654}}},{"Increment":{"amount":1,"offset":2,"position":{"start":6684,"end":6684}}},{"PointerIncrement":{"amount":2,"position":{"start":6681,"end":6682}}},{"Loop":{"body":[{"Increment":{"amount":-1,"offset":0,"position":{"start":6688,"end":6688}}},{"Increment":{"amount":1,"offset":2,"position":{"start":6693,"end":6693}}},{"PointerIncrement":{"amount":2,"position":{"start":6690,"end":6691}}}],"position":{"start":6686,"end":6695}}},{"Set":{"amount":-1,"offset":0,"position":{"start":6697,"end":6697}}},{"Loop":{"body":[{"PointerIncrement":{"amount":2,"position":{"start":6733,"end":6734}}}],"position":{"start":6729,"end":6738}}},{"Increment":{"amount":-1,"offset":1,"position":{"start":6794,"end":6794}}}],"position":{"start":6013,"end":6840}}},{"PointerIncrement":{"amount":1,"position":{"start":6845,"end":6845}}},{"Loop":{"body":[{"Increment":{"amount":7,"offset":0,"position":{"start":6912,"end":6914}}},{"MultiplyMove":{"changes":{"-1":12},"position":{"start":6921,"end":6965}}},{"Increment":{"amount":2,"offset":-1,"position":{"start":6969,"end":6970}}}],"position":{"start":6847,"end":7041}}},{"Increment":{"amount":1,"offset":-3,"position":{"start":7179,"end":7179}}},{"Increment":{"amount":-1,"offset":-1,"position":{"start":7048,"end":7048}}},{"PointerIncrement":{"amount":-3,"position":{"start":7176,"end":7177}}},{"Loop":{"body":[{"Increment":{"amount":1,"offset":-2,"position":{"start":7188,"end":7188}}},{"Increment":{"amount":-1,"offset":0,"position":{"start":7183,"end":7183}}},{"PointerIncrement":{"amount":-2,"position":{"start":7185,"end":7186}}}],"position":{"start":7181,"end":7190}}},{"Increment":{"amount":1,"offset":-2,"position":{"start":7224,"end":7224}}},{"Set":{"amount":-1,"offset":0,"position":{"start":7192,"end":7192}}},{"PointerIncrement":{"amount":-2,"position":{"start":7221,"end":7222}}},{"Loop":{"body":[{"Increment":{"amount":1,"offset":-2,"position":{"start":7233,"end":7233}}},{"Increment":{"amount":-1,"offset":0,"position":{"start":7228,"end":7228}}},{"PointerIncrement":{"amount":-2,"position":{"start":7230,"end":7231}}}],"position":{"start":7226,"end":7235}}},{"Increment":{"amount":1,"offset":-2,"position":{"start":7269,"end":7269}}},{"Set":{"amount":-1,"offset":0,"position":{"start":7237,"end":7237}}},{"PointerIncrement":{"amount":-2,"position":{"start":7266,"end":7267}}},{"Loop":{"body":[{"Increment":{"amount":1,"offset":-2,"position":{"start":7278,"end":7278}}},{"Increment":{"amount":-1,"offset":0,"position":{"start":7273,"end":7273}}},{"PointerIncrement":{"amount":-2,"position":{"start":7275,"end":7276}}}],"position":{"start":7271,"end":7280}}},{"Increment":{"amount":1,"offset":-2,"position":{"start":7314,"end":7314}}},{"Set":{"amount":-1,"offset":0,"position":{"start":7282,"end":7282}}},{"PointerIncrement":{"amount":-2,"position":{"start":7311,"end":7312}}},{"Loop":{"body":[{"Increment":{"amount":1,"offset":-2,"position":{"start":7323,"end":7323}}},{"Increment":{"amount":-1,"offset":0,"position":{"start":7318,"end":7318}}},{"PointerIncrement":{"amount":-2,"position":{"start":7320,"end":7321}}}],"position":{"start":7316,"end":7325}}},{"Increment":{"amount":-1,"offset":-8,"position":{"start":7419,"end":7419}}},{"Increment":{"amount":1,"offset":-2,"position":{"start":7410,"end":7410}}},{"Set":{"amount":-1,"offset":0,"position":{"start":7327,"end":7327}}},{"PointerIncrement":{"amount":-2,"position":{"start":7421,"end":7426}}},{"MultiplyMove":{"changes":{"-6":1},"position":{"start":7468,"end":7488}}},{"Set":{"amount":-1,"offset":-4,"position":{"start":7553,"end":7556}}},{"Increment":{"amount":1,"offset":2,"position":{"start":7544,"end":7544}}},{"Increment":{"amount":1,"offset":4,"position":{"start":7608,"end":7608}}},{"PointerIncrement":{"amount":4,"position":{"start":7605,"end":7606}}},{"Loop":{"body":[{"Increment":{"amount":-1,"offset":0,"position":{"start":7612,"end":7612}}},{"Increment":{"amount":1,"offset":2,"position":{"start":7617,"end":7617}}},{"PointerIncrement":{"amount":2,"position":{"start":7614,"end":7615}}}],"position":{"start":7610,"end":7619}}},{"Set":{"amount":-1,"offset":-4,"position":{"start":7657,"end":7660}}},{"Increment":{"amount":1,"offset":2,"position":{"start":7717,"end":7717}}},{"PointerIncrement":{"amount":2,"position":{"start":7714,"end":7715}}},{"Loop":{"body":[{"Increment":{"amount":-1,"offset":0,"position":{"start":7721,"end":7721}}},{"Increment":{"amount":1,"offset":2,"position":{"start":7726,"end":7726}}},{"PointerIncrement":{"amount":2,"position":{"start":7723,"end":7724}}}],"position":{"start":7719,"end":7728}}},{"Increment":{"amount":1,"offset":-4,"position":{"start":7825,"end":7825}}},{"Set":{"amount":-1,"offset":-2,"position":{"start":7764,"end":7767}}},{"PointerIncrement":{"amount":-4,"position":{"start":7822,"end":7823}}},{"Loop":{"body":[{"Increment":{"amount":1,"offset":-2,"position":{"start":7834,"end":7834}}},{"Increment":{"amount":-1,"offset":0,"position":{"start":7829,"end":7829}}},{"PointerIncrement":{"amount":-2,"position":{"start":7831,"end":7832}}}],"position":{"start":7827,"end":7836}}},{"Increment":{"amount":1,"offset":-2,"position":{"start":7870,"end":7870}}},{"Set":{"amount":-1,"offset":0,"position":{"start":7838,"end":7838}}},{"PointerIncrement":{"amount":-2,"position":{"start":7867,"end":7868}}},{"Loop":{"body":[{"Increment":{"amount":1,"offset":-2,"position":{"start":7879,"end":7879}}},{"Increment":{"amount":-1,"offset":0,"position":{"start":7874,"end":7874}}},{"PointerIncrement":{"amount":-2,"position":{"start":7876,"end":7877}}}],"position":{"start":7872,"end":7881}}},{"Increment":{"amount":-1,"offset":-2,"position":{"start":8034,"end":8034}}},{"Set":{"amount":-1,"offset":0,"position":{"start":7883,"end":7883}}},{"PointerIncrement":{"amount":-2,"position":{"start":7971,"end":7972}}},{"Loop":{"body":[{"Increment":{"amount":1,"offset":0,"position":{"start":8120,"end":8120}}},{"Increment":{"amount":1,"offset":1,"position":{"start":8124,"end":8124}}}],"position":{"start":8036,"end":8131}}},{"Set":{"amount":1,"offset":0,"position":{"start":8133,"end":8133}}},{"PointerIncrement":{"amount":1,"position":{"start":8140,"end":8140}}},{"Loop":{"body":[{"MultiplyMove":{"changes":{"-2":1},"position":{"start":8244,"end":8256}}},{"Set":{"amount":0,"offset":-1,"position":{"start":8328,"end":8330}}}],"position":{"start":8142,"end":8385}}},{"PointerIncrement":{"amount":-1,"position":{"start":8390,"end":8390}}}],"position":{"start":1704,"end":8447}}},{"Increment":{"amount":-1,"offset":-1,"position":{"start":8520,"end":8520}}},{"PointerIncrement":{"amount":-1,"position":{"start":8518,"end":8518}}},{"Write":{"position":{"start":8522,"end":8522}}}]"#;

    
    let instrs = serde_json::from_str::<Vec<AstNode>>(read_str).unwrap();
    let serialized = instrs.serialize_to_turing();
    assert!(!serialized.is_empty());
}

#[test]
fn two_state_test() {

}