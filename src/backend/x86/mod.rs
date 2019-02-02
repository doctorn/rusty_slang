use super::super::frontend::ast::{BinOp, UnOp};
use super::Expr;
use std::fmt;
use std::sync::atomic::{AtomicUsize, Ordering};

static LABEL_COUNT: AtomicUsize = AtomicUsize::new(0);

struct Label(usize);

impl Label {
    fn new() -> Label {
        Label(LABEL_COUNT.fetch_add(1, Ordering::SeqCst))
    }
}

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, ".L{}", self.0)
    }
}

fn emit_preamble(asm: &mut Vec<String>) {
    asm.push("  .data".to_string()); // TODO move to runtime
    asm.push("format: .asciz \"%d\\n\"".to_string()); // TODO move to runtime
    asm.push("  .text".to_string());
    asm.push("  .extern printf".to_string()); // TODO move to runtime
    asm.push("  .extern alloc".to_string());
    asm.push("  .extern make_inl".to_string());
    asm.push("  .extern make_inr".to_string());
    asm.push("  .extern make_pair".to_string());
    asm.push("  .extern fst".to_string());
    asm.push("  .extern snd".to_string());
    asm.push("  .extern make_ref".to_string());
    asm.push("  .extern deref".to_string());
    asm.push("  .extern assign".to_string());
    asm.push("  .extern make_closure".to_string());
    asm.push("  .extern make_recursive_closure".to_string());
    asm.push("  .extern apply".to_string());
    asm.push("  .extern check_case".to_string());
    asm.push("  .globl  main".to_string());
    asm.push("  .type main, @function".to_string());
    asm.push("main:".to_string());
    asm.push("  pushq %rbp".to_string());
    asm.push("  movq %rsp,%rbp".to_string());
}

fn emit_recursive_lambda(
    rec: String,
    v: String,
    sub: Expr,
    asm: &mut Vec<String>,
    env: &Vec<String>,
) {
    let skip = Label::new();
    let f = Label::new();
    asm.push(format!("  jmp {}", skip));
    asm.push(format!("{}:", f));
    asm.push("  pushq %rbp".to_string());
    asm.push("  movq %rsp,%rbp".to_string());
    let mut closure_env = vec![v];
    asm.push("  movq %rdi,-8(%rbp)".to_string());
    // TODO constrain to free variables
    for (i, envv) in vec![rec].iter().chain(env.iter()).enumerate() {
        closure_env.push(envv.clone());
        asm.push(format!("  movq {}(%rsi),%rax", 8 * i));
        asm.push(format!("  movq %rax,-{}(%rbp)", 8 * (i + 2)));
    }
    asm.push(format!("  subq ${},%rsp", closure_env.len() * 8));
    emit_asm(sub, asm, &mut closure_env);
    asm.push("  movq %rbp,%rsp".to_string());
    asm.push("  popq %rbp".to_string());
    asm.push("  ret".to_string());
    asm.push(format!("{}:", skip));
    asm.push(format!("  leaq {}(%rip),%rdi", f));
    // TODO only capture free variables
    for i in (1..=env.len()).rev() {
        match i {
            1 => asm.push(format!("  movq -{}(%rbp),%rdx", 8 * i)),
            2 => asm.push(format!("  movq -{}(%rbp),%rcx", 8 * i)),
            3 => asm.push(format!("  movq -{}(%rbp),%r8", 8 * i)),
            4 => asm.push(format!("  movq -{}(%rbp),%r9", 8 * i)),
            _ => asm.push(format!("  pushq -{}(%rbp)", 8 * i)),
        }
    }
    asm.push(format!("  movq ${},%rsi", env.len()));
    asm.push("  xorq %rax,%rax".to_string());
    asm.push("  call make_recursive_closure".to_string());
    asm.push(format!(
        "  addq ${},%rsp",
        8 * std::cmp::max(env.len() as i64 - 5, 0)
    ));
}

fn emit_lambda(v: String, sub: Expr, asm: &mut Vec<String>, env: &Vec<String>) {
    let skip = Label::new();
    let f = Label::new();
    asm.push(format!("  jmp {}", skip));
    asm.push(format!("{}:", f));
    asm.push("  pushq %rbp".to_string());
    asm.push("  movq %rsp,%rbp".to_string());
    let mut closure_env = vec![v];
    asm.push("  movq %rdi,-8(%rbp)".to_string());
    // TODO constrain to free variables
    for (i, envv) in env.iter().enumerate() {
        closure_env.push(envv.clone());
        asm.push(format!("  movq {}(%rsi),%rax", 8 * i));
        asm.push(format!("  movq %rax,-{}(%rbp)", 8 * (i + 2)));
    }
    asm.push(format!("  subq ${},%rsp", closure_env.len() * 8));
    emit_asm(sub, asm, &mut closure_env);
    asm.push("  movq %rbp,%rsp".to_string());
    asm.push("  popq %rbp".to_string());
    asm.push("  ret".to_string());
    asm.push(format!("{}:", skip));
    asm.push(format!("  leaq {}(%rip),%rdi", f));
    // TODO only capture free variables
    for i in (1..=env.len()).rev() {
        match i {
            1 => asm.push(format!("  movq -{}(%rbp),%rdx", 8 * i)),
            2 => asm.push(format!("  movq -{}(%rbp),%rcx", 8 * i)),
            3 => asm.push(format!("  movq -{}(%rbp),%r8", 8 * i)),
            4 => asm.push(format!("  movq -{}(%rbp),%r9", 8 * i)),
            _ => asm.push(format!("  pushq -{}(%rbp)", 8 * i)),
        }
    }
    asm.push(format!("  movq ${},%rsi", env.len()));
    asm.push("  xorq %rax,%rax".to_string());
    asm.push("  call make_closure".to_string());
    asm.push(format!(
        "  addq ${},%rsp",
        8 * std::cmp::max(env.len() as i64 - 5, 0)
    ));
}

fn emit_asm(expr: Expr, asm: &mut Vec<String>, env: &Vec<String>) {
    use Expr::*;
    match expr {
        Constant(i) => asm.push(format!("  movq ${},%rax", i)),
        Var(v) => {
            let position = env.iter().position(|u| u == &v).unwrap();
            asm.push(format!("  movq -{}(%rbp),%rax", 8 * (1 + position)));
        }
        UnOp(op, sub) => {
            emit_asm(*sub, asm, env);
            use self::UnOp::*;
            match op {
                Neg => asm.push("  negq %rax".to_string()),
                Not => asm.push("  notq %rax".to_string()),
                _ => unimplemented!(),
            }
        }
        BinOp(op, left, right) => {
            use self::BinOp::*;
            match op {
                And => {
                    let label = Label::new();
                    emit_asm(*left, asm, env);
                    asm.push("  cmp $1,%rax".to_string());
                    asm.push(format!("  jne {}", label));
                    emit_asm(*right, asm, env);
                    asm.push(format!("{}:", label));
                }
                Or => {
                    let label = Label::new();
                    emit_asm(*left, asm, env);
                    asm.push("  cmpq $1,%rax".to_string());
                    asm.push(format!("  je {}", label));
                    emit_asm(*right, asm, env);
                    asm.push(format!("{}:", label));
                }
                _ => {
                    emit_asm(*left, asm, env);
                    asm.push("  pushq %rax".to_string());
                    emit_asm(*right, asm, env);
                    asm.push("  movq %rax,%rbx".to_string());
                    asm.push("  popq %rax".to_string());
                    match op {
                        Add => asm.push("  addq %rbx,%rax".to_string()),
                        Mul => asm.push("  imulq %rbx,%rax".to_string()),
                        Sub => asm.push("  subq %rbx,%rax".to_string()),
                        Div => {
                            asm.push("  cqto".to_string());
                            asm.push("  idivq %rbx".to_string());
                        }
                        Lt => {
                            let false_label = Label::new();
                            let exit_label = Label::new();
                            asm.push("  cmpq %rbx,%rax".to_string());
                            asm.push(format!("  jge {}", false_label));
                            asm.push("  movq $1,%rax".to_string());
                            asm.push(format!("  jmp {}", exit_label));
                            asm.push(format!("{}:", false_label));
                            asm.push("  movq $0,%rax".to_string());
                            asm.push(format!("{}:", exit_label));
                        }
                        Eq => {
                            let false_label = Label::new();
                            let exit_label = Label::new();
                            asm.push("  cmpq %rbx,%rax".to_string());
                            asm.push(format!("  jne {}", false_label));
                            asm.push("  movq $1,%rax".to_string());
                            asm.push(format!("  jmp {}", exit_label));
                            asm.push(format!("{}:", false_label));
                            asm.push("  movq $0,%rax".to_string());
                            asm.push(format!("{}:", exit_label));
                        }
                        _ => unreachable!(),
                    }
                }
            }
        }
        If(condition, left, right) => {
            let false_label = Label::new();
            let exit_label = Label::new();
            emit_asm(*condition, asm, env);
            asm.push("  cmpq $1,%rax".to_string());
            asm.push(format!("  jne {}", false_label));
            emit_asm(*left, asm, env);
            asm.push(format!("  jmp {}", exit_label));
            asm.push(format!("{}:", false_label));
            emit_asm(*right, asm, env);
            asm.push(format!("{}:", exit_label));
        }
        While(condition, sub) => {
            let loop_label = Label::new();
            let exit_label = Label::new();
            asm.push(format!("{}:", loop_label));
            emit_asm(*condition, asm, env);
            asm.push("  cmpq $1,%rax".to_string());
            asm.push(format!("  jne {}", exit_label));
            emit_asm(*sub, asm, env);
            asm.push(format!("  jmp {}", loop_label));
            asm.push(format!("{}:", exit_label));
        }
        Seq(seq) => {
            for expr in seq.into_iter() {
                emit_asm(expr, asm, env);
            }
        }
        Pair(left, right) => {
            emit_asm(*left, asm, env);
            asm.push("  pushq %rax".to_string());
            emit_asm(*right, asm, env);
            asm.push("  popq %rdi".to_string());
            asm.push("  movq %rax,%rsi".to_string());
            asm.push("  xorq %rax,%rax".to_string());
            asm.push("  call make_pair".to_string());
        }
        Fst(sub) => {
            emit_asm(*sub, asm, env);
            asm.push("  movq %rax,%rdi".to_string());
            asm.push("  xorq %rax,%rax".to_string());
            asm.push("  call fst".to_string());
        }
        Snd(sub) => {
            emit_asm(*sub, asm, env);
            asm.push("  movq %rax,%rdi".to_string());
            asm.push("  xorq %rax,%rax".to_string());
            asm.push("  call snd".to_string());
        }
        Inl(sub) => {
            emit_asm(*sub, asm, env);
            asm.push("  movq %rax,%rdi".to_string());
            asm.push("  xorq %rax,%rax".to_string());
            asm.push("  call make_inl".to_string());
        }
        Inr(sub) => {
            emit_asm(*sub, asm, env);
            asm.push("  movq %rax,%rdi".to_string());
            asm.push("  xorq %rax,%rax".to_string());
            asm.push("  call make_inr".to_string());
        }
        Ref(sub) => {
            emit_asm(*sub, asm, env);
            asm.push("  movq %rax,%rdi".to_string());
            asm.push("  xorq %rax,%rax".to_string());
            asm.push("  call make_ref".to_string());
        }
        Deref(sub) => {
            emit_asm(*sub, asm, env);
            asm.push("  movq %rax,%rdi".to_string());
            asm.push("  xorq %rax,%rax".to_string());
            asm.push("  call deref".to_string());
        }
        Assign(left, right) => {
            emit_asm(*left, asm, env);
            asm.push("  pushq %rax".to_string());
            emit_asm(*right, asm, env);
            asm.push("  popq %rdi".to_string());
            asm.push("  movq %rax,%rsi".to_string());
            asm.push("  xorq %rax,%rax".to_string());
            asm.push("  call assign".to_string());
            asm.push("  xorq %rax,%rax".to_string());
        }
        Lambda(v, sub) => {
            emit_lambda(v, *sub, asm, env);
        }
        App(left, right) => {
            emit_asm(*left, asm, env);
            asm.push("  pushq %rax".to_string());
            emit_asm(*right, asm, env);
            asm.push("  popq %rdi".to_string());
            asm.push("  movq %rax,%rsi".to_string());
            asm.push("  xorq %rax,%rax".to_string());
            asm.push("  call apply".to_string());
        }
        Case(sub, (v_left, lambda_left), (v_right, lambda_right)) => {
            emit_asm(*sub, asm, env);
            asm.push("  pushq %rax".to_string());
            emit_lambda(v_left, *lambda_left, asm, env);
            asm.push("  pushq %rax".to_string());
            emit_lambda(v_right, *lambda_right, asm, env);
            asm.push("  movq %rax,%rdx".to_string());
            asm.push("  popq %rsi".to_string());
            asm.push("  popq %rdi".to_string());
            asm.push("  xorq %rax,%rax".to_string());
            asm.push("  call check_case".to_string());
        }
        LetFun(f, (v, sub), body) => {
            emit_lambda(f.clone(), *body, asm, env);
            asm.push("  pushq %rax".to_string());
            emit_recursive_lambda(f, v, *sub, asm, env);
            asm.push("  popq %rdi".to_string());
            asm.push("  movq %rax,%rsi".to_string());
            asm.push("  xorq %rax,%rax".to_string());
            asm.push("  call apply".to_string());
        }
    }
}

pub fn to_x86(expr: Expr) -> Vec<String> {
    let mut asm = vec![];
    let env = vec![];
    emit_preamble(&mut asm);
    emit_asm(expr, &mut asm, &env);

    asm.push("  mov %rax,%rsi".to_string()); // TODO move to runtime
    asm.push("  leaq format(%rip),%rdi".to_string()); // TODO move to runtime
    asm.push("  xorq %rax,%rax".to_string()); // TODO move to runtime
    asm.push("  call printf".to_string()); // TODO move to runtime

    asm.push("  popq %rbp".to_string());
    asm.push("  ret".to_string());
    asm
}
