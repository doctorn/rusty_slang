use super::frontend::ast::{BinOp, Expr, Free, UnOp};

mod x86;

use x86::*;

use std::fmt;

struct Generator {
    functions: Vec<GeneratedCode>,
}

impl Generator {
    fn new() -> Generator {
        Generator { functions: vec![] }
    }

    fn add(&mut self, code: GeneratedCode) {
        self.functions.push(code)
    }
}

impl fmt::Display for Generator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "\t.text")?;
        writeln!(f, "\t.extern alloc")?;
        writeln!(f, "\t.extern make_closure")?;
        writeln!(f, "\t.extern make_recursive_closure")?;
        writeln!(f, "\t.extern what")?;
        writeln!(f, "\t.globl entry")?;
        writeln!(f, "\t.type entry, @function")?;
        for function in self.functions.iter() {
            write!(f, "{}", function)?;
        }
        Ok(())
    }
}

impl Code {
    fn emit_var(&mut self, v: String) -> &mut Code {
        let loc = self.get(&v);
        self.comment(format!(
            "'{}' is in '{}' so move it to the accumulator ('{}')",
            v,
            loc,
            rax()
        ))
        .mov(loc, rax())
    }

    fn emit_unop(&mut self, op: UnOp, expr: Expr, generator: &mut Generator) -> &mut Code {
        use self::UnOp::*;
        self.comment(format!(
            "compute the operand for '{}' and leave it in the accumulator ('{}')",
            op,
            rax()
        ))
        .emit(expr, generator)
        .comment(format!(
            "apply the '{}' and leave the result in the accumulator ('{}')",
            op,
            rax()
        ));
        match op {
            Neg => self.neg(rax()),
            Not => self.not(rax()),
        }
    }

    fn emit_binop(
        &mut self,
        op: BinOp,
        left: Expr,
        right: Expr,
        generator: &mut Generator,
    ) -> &mut Code {
        use self::BinOp::*;
        match op {
            And => {
                let label = Label::new();
                self.comment(format!(
                    "for '&&' we only evaluate the right operand if the left evaluated to 'true'"
                ))
                .comment(format!("compute the left operand for the '&&'"))
                .emit(left, generator)
                .comment(format!(
                    "the value of the left operand of the '&&' is left in the accumulator ('{}')",
                    rax()
                ))
                .comment(format!(
                    "we compare this with the binary encoding of 'true'"
                ))
                .cmp(constant(1), rax())
                .comment(format!(
                    "if the value in the accumulator ('{}') is not true we jump passed the right operand to '{}'", rax(), label
                ))
                .jne(label)
                .comment(format!("if we did not make the jump, the result of the '&&' should be the result of the right operand"))
                .emit(right, generator)
                .comment(format!(
                    "the value of the right operand of the '&&' is left in the accumulator ('{}')",
                    rax()
                ))
                .comment(format!("this becomes the value of the '&&'"))
                .label(label)
            }
            Or => {
                let label = Label::new();
                self.emit(left, generator)
                    .cmp(constant(1), rax())
                    .je(label)
                    .emit(right, generator)
                    .label(label)
            }
            _ => {
                self.comment(format!("compute the operands for the '{}'", op))
                    .emit(left, generator)
                    .comment(format!("the value of the left operand of the '{}' is left in the accumulator ('{}') so we save this", op, rax()))
                    .push(rax())
                    .emit(right, generator)
                    .comment(format!("the value of the right operand of the '{}' is left in the accumulator ('{}')", op, rax()))
                    .comment(format!("move this into '{}' to make way for the first operand", rbx()))
                    .mov(rax(), rbx())
                    .comment(format!("now restore the first operand to the accumulator ('{}')", rax()))
                    .pop(rax());
                match op {
                    Add => self.comment(format!("for the '+', add the value in '{}' to the value in '{}' and leave the result in the accumulator ('{}')", rbx(), rax(), rax())).add(rbx(), rax()),
                    Sub => self.comment(format!("for the '-', subtract the value in '{}' from the value in '{}' and leave the result in the accumulator ('{}')", rbx(), rax(), rax())).sub(rbx(), rax()),
                    Mul => self.comment(format!("for the '*', multiply the value in '{}' by the value in '{}' and leave the result in the accumulator ('{}')", rax(), rbx(), rax())).mul(rbx(), rax()),
                    Div => self
                        .comment(format!(
                            "sign extend the accumulator ('{}') into '{}'",
                            rax(),
                            rdx()
                        ))
                        .cqto()
                        .comment(format!("for the '/', divide '{}:{}' by '{}' and leave the result in '{}:{}'", rdx(), rax(), rbx(), rdx(), rax()))
                        .div(rbx()),
                    Lt => {
                        let false_label = Label::new();
                        let exit_label = Label::new();
                        self.comment(format!("for '<' we compare the values in '{}' and '{}' and branch depending on the result", rax(), rbx())).cmp(rbx(), rax())
                            .comment(format!("if '{}' >= '{}', we jump to '{}'", rbx(), rax(), false_label))
                            .jge(false_label)
                            .comment(format!("if we don't make the jump, move the binary encoding of 'true' into the accumulator ('{}')", rax()))
                            .mov(constant(1), rax())
                            .comment(format!("jump over '{}' so that we don't overwrite the accumulator ('{}')", false_label, rax()))
                            .jmp(exit_label)
                            .label(false_label)
                            .comment(format!("we've made the jump so move the binary encoding of 'false' into the accumulator ('{}')", rax()))
                            .mov(constant(0), rax())
                            .label(exit_label)
                    }
                    Eq => {
                        let false_label = Label::new();
                        let exit_label = Label::new();
                        self.comment(format!("for '=' we compare the values in '{}' and '{}' and branch depending on the result", rax(), rbx())).cmp(rbx(), rax())
                            .comment(format!("if the values are unequal, we jump to '{}'", false_label))
                            .jne(false_label)
                            .comment(format!("if we don't make the jump, move the binary encoding of 'true' into the accumulator ('{}')", rax()))
                            .mov(constant(1), rax())
                            .comment(format!("jump over '{}' so that we don't overwrite the accumulator ('{}')", false_label, rax()))
                            .jmp(exit_label)
                            .label(false_label)
                            .comment(format!("we've made the jump so move the binary encoding of 'false' into the accumulator ('{}')", rax()))
                            .mov(constant(0), rax())
                            .label(exit_label)
                    }
                    _ => unreachable!(),
                }
            }
        }
    }

    fn emit_if(
        &mut self,
        condition: Expr,
        left: Expr,
        right: Expr,
        generator: &mut Generator,
    ) -> &mut Code {
        let false_label = Label::new();
        let exit_label = Label::new();
        self.comment(format!(
            "compute the condition value for the 'if' expression"
        ))
        .emit(condition, generator)
        .comment(format!(
            "the condition value for the 'if' expression is left in the accumulator ('{}')",
            rax()
        ))
        .comment(format!(
            "test if this is equal to the binary encoding for 'true'"
        ))
        .cmp(constant(1), rax())
        .comment(format!("if it is not, we jump to '{}'", false_label))
        .jne(false_label)
        .comment(format!(
            "if we haven't made the jump, we continue the computation for the 'true' branch"
        ))
        .emit(left, generator)
        .comment(format!("when we're done, jump over the 'false' branch"))
        .jmp(exit_label)
        .label(false_label)
        .comment(format!(
            "if we have made the jump, we continue the computation for the 'false' branch"
        ))
        .emit(right, generator)
        .label(exit_label)
    }

    fn emit_while(&mut self, condition: Expr, sub: Expr, generator: &mut Generator) -> &mut Code {
        let loop_label = Label::new();
        let exit_label = Label::new();
        self.label(loop_label)
            .comment(format!("compute the condition value for the 'while' loop"))
            .emit(condition, generator)
            .comment(format!(
                "the condition value for the 'while' loop is left in the accumulator ('{}')",
                rax()
            ))
            .comment(format!(
                "test if this is equal to the binary encoding for 'true'"
            ))
            .cmp(constant(1), rax())
            .comment(format!(
                "if it is not, we jump to '{}' as we have finished the loop",
                exit_label
            ))
            .jne(exit_label)
            .comment(format!(
                "if we haven't made the jump, recompute the body of the while loop"
            ))
            .emit(sub, generator)
            .comment(format!(
                "once we've recomputed the body of the loop, we jump back to '{}' to loop again",
                loop_label
            ))
            .jmp(loop_label)
            .label(exit_label)
    }

    fn emit_seq(&mut self, seq: Vec<Expr>, generator: &mut Generator) -> &mut Code {
        for (i, sub) in seq.into_iter().enumerate() {
            if i > 0 {
                self.comment(format!("we move onto the next instruction in a sequence here, so we simply ignore the value in the accumulator ('{}')", rax()));
            }
            self.emit(sub, generator);
        }
        self
    }

    fn emit_ref(&mut self, sub: Expr, generator: &mut Generator) -> &mut Code {
        self.comment(format!("compute the value that we want to reference"))
            .emit(sub, generator)
            .comment(format!(
                "the value we want to reference is left in the accumulator ('{}') so we save this",
                rax()
            ))
            .push(rax())
            .comment(format!(
                "to build the reference, we need to call into the runtime"
            ))
            .comment(format!(
                "empty '{}' as the C runtime expects this to be 0",
                rax()
            ))
            .xor(rax(), rax())
            .comment(format!("actually call the runtime"))
            .call_rt("alloc")
            .comment(format!(
                "the newly constructed heap pointer will be left in the accumulator ('{}')",
                rax()
            ))
            .comment(format!(
                "restore the value we wanted to reference into its new heap location ('{}')",
                deref(rax(), 0)
            ))
            .pop(deref(rax(), 0))
    }

    fn emit_pair(&mut self, left: Expr, right: Expr, generator: &mut Generator) -> &mut Code {
        self.comment(format!("compute the left hand value for the pair"))
            .emit(left, generator)
            .comment(format!(
                "the left hand value for the pair is left in the accumulator ('{}') so we save this",
                rax()
            ))
            .push(rax())
            .comment(format!("compute the right hand value for the pair"))
            .emit(right, generator)
            .comment(format!(
                "the right hand value for the pair is left in the accumulator ('{}') so we save this",
                rax()
            ))
            .push(rax())
            .comment(format!("to build the pair we need to call into the runtime"))
            .comment(format!(
                "empty '{}' as the C runtime expects this to be 0",
                rax()
            ))
            .xor(rax(), rax())
            .comment(format!("actually call the runtime"))
            .call_rt("alloc")
            .comment(format!("the heap pointer for the pair will be left in the accumulator ('{}')", rax()))
            .comment(format!("we recover the right hand value and move it into the heap ('{}')", deref(rax(), 8)))
            .pop(deref(rax(), 8))
            .comment(format!("we recover the left hand value and move it into the heap ('{}')", deref(rax(), 8)))
            .pop(deref(rax(), 0))
    }

    fn emit_assign(&mut self, left: Expr, right: Expr, generator: &mut Generator) -> &mut Code {
        self.comment(format!("compute the reference to assign to"))
            .emit(left, generator)
            .comment(format!(
                "the reference for the assignment is left in the accumulator ('{}') so we save this",
                rax()
            ))
            .push(rax())
            .comment(format!("compute the value we want to assign"))
            .emit(right, generator)
            .comment(format!(
                "the value for the assignment is left in the accumulaotr ('{}')",
                rax()
            ))
            .comment(format!("recover the reference and leave it in '{}'", rbx()))
            .pop(rbx())
            .comment(format!(
                "move the value in the accumulator ('{}') into the referenced memory location)",
                rax()
            ))
            .mov(rax(), deref(rbx(), 0))
            .comment(format!(
                "after assignment we zero the accumulator to represent returning ()"
            ))
            .xor(rax(), rax())
    }

    fn emit_app(&mut self, left: Expr, right: Expr, generator: &mut Generator) -> &mut Code {
        self.comment(format!("get the closure pointer for the application"))
            .emit(left, generator)
            .comment(format!(
                "the closure pointer is left in the accumulator ('{}'), so we save this",
                rax()
            ))
            .push(rax())
            .comment(format!("compute the argument for the application"))
            .emit(right, generator)
            .comment(format!(
                "the argument is left in the accumulator ('{}')",
                rax()
            ))
            .comment(format!(
                "move this into '{}' as this is where our closure will expect it to be",
                rdi()
            ))
            .mov(rax(), rdi())
            .comment(format!(
                "restore the closure pointer to the accumulator ('{}')",
                rax()
            ))
            .pop(rax())
            .comment(format!("move the pointer to the closure's environment from '{}' into '{}' as this is where it will expect it to be", deref(rax(), 8), rsi()))
            .mov(deref(rax(), 8), rsi())
            .comment(format!("move the the code pointer for the closure from '{}' into the accumulator ('{}')", deref(rax(), 0), rax()))
            .mov(deref(rax(), 0), rax())
            .comment(format!("call the closure"))
            .call(rax())
    }

    fn emit_lambda(&mut self, lambda: (String, Box<Expr>), generator: &mut Generator) -> &mut Code {
        let fv = lambda
            .fv()
            .into_iter()
            .map(|x| x.clone())
            .collect::<Vec<_>>();
        let (v, expr) = (lambda.0, *lambda.1);
        let label = Label::new();
        let mut lambda = Code::new(label);
        lambda.comment(format!(
            "the formal parameter of the function will be left in '{}' and a pointer to the closure's environment will be left in '{}'", rdi(), rsi()
        ));
        let vloc = lambda.allocate(v.clone());
        if fv.len() > 0 {
            lambda.comment(format!(
                "now each of the free variables of the closure is moved into our stack frame"
            ));
        }
        for (i, envv) in fv.iter().enumerate() {
            let loc = lambda.allocate(envv.clone());
            lambda
                .comment(format!(
                    "'{}' is loaded from the environment ('{}') into its allocated space ('{}')",
                    envv,
                    deref(rsi(), 8 * (i + 1) as i64),
                    loc
                ))
                .mov(deref(rsi(), 8 * i as i64), rax())
                .mov(rax(), loc);
        }
        lambda
            .comment(format!(
                "move the formal parameter '{}' from '{}' into its allocated space ('{}')",
                v,
                rdi(),
                vloc
            ))
            .mov(rdi(), vloc)
            .emit(expr, generator);
        generator.add(lambda.ret());
        self.comment(format!(
            "to construct the closure, we need to pass the enviroment to the runtime"
        ));
        for (i, envv) in fv.iter().enumerate().rev() {
            let loc = self.get(&envv);
            match i {
                0 => self
                    .comment(format!("'{}' ('{}') is moved to '{}'", envv, loc, rdx()))
                    .mov(loc, rdx()),
                1 => self
                    .comment(format!("'{}' ('{}') is moved to '{}'", envv, loc, rcx()))
                    .mov(loc, rcx()),
                2 => self
                    .comment(format!("'{}' ('{}') is moved to '{}'", envv, loc, r8()))
                    .mov(loc, r8()),
                3 => self
                    .comment(format!("'{}' ('{}') is moved to '{}'", envv, loc, r9()))
                    .mov(loc, r9()),
                _ => self
                    .comment(format!("'{}' ('{}') is pushed to the stack", envv, loc))
                    .push(loc),
            };
        }
        let env_len = lambda.get_env().len() - 1;
        self.comment(format!(
            "now we place a pointer to the code for the closure in '{}'",
            rdi()
        ))
        .comment(format!(
            "note how 'leaq' is used relative to the program counter ('{}')",
            rip()
        ))
        .lea(relative(rip(), label), rdi())
        .comment(format!("we also need to let the runtime know how many variables are in our environment (in this case {})", env_len))
        .comment(format!("this information is placed in '{}'", rsi()))
        .mov(constant(env_len as i64), rsi())
        .comment(format!(
            "empty '{}' as the C runtime expects this to be 0",
            rax()
        ))
        .xor(rax(), rax())
        .comment(format!("actually call into the runtime"))
        .call_rt("make_closure");
        if env_len > 4 {
            let allocated = (env_len - 4) * 8;
            self.add(constant(allocated as i64), rsp())
                .comment(format!("while building the closure we pushed {} bytes onto the stack that we no longer need, so we increment '{}' by {}", allocated, rsp(), allocated)).add(constant(allocated as i64), rsp())
        } else {
            self
        }
    }

    fn emit_recursive_lambda(
        &mut self,
        f: String,
        lambda: (String, Box<Expr>),
        generator: &mut Generator,
    ) -> &mut Code {
        let fv = lambda
            .fv()
            .into_iter()
            .map(|x| x.clone())
            .filter(|x| x != &f)
            .collect::<Vec<_>>();
        let (v, expr) = (lambda.0, *lambda.1);
        let label = Label::new();
        let mut lambda = Code::new(label);
        let vloc = lambda.allocate(v.clone());
        let floc = lambda.allocate(f.clone());
        lambda
            .comment(format!(
                "this is the emitted code for the function '{}'"
            , f))
            .comment(format!(
                "the formal parameter of the function will be left in '{}' and a pointer to the closure's environment will be left in '{}'", rdi(), rsi()
            ))
            .comment(format!("as this is a recursive function, the first item in the environment will be a pointer to the closure itself"))
            .comment(format!("load this into the accumulator ('{}') from '{}'", rax(), deref(rsi(), 0)))
            .mov(deref(rsi(), 0), rax())
            .comment(format!("move '{}'s closure pointer into its allocated space ('{}')", f, floc))
            .mov(rax(), floc);
        if fv.len() > 0 {
            lambda.comment(format!(
                "now each of the free variables of the closure is moved into our stack frame"
            ));
        }
        for (i, envv) in fv.iter().enumerate() {
            let loc = lambda.allocate(envv.clone());
            lambda
                .comment(format!(
                    "'{}' is loaded from the environment ('{}') into its allocated space ('{}')",
                    envv,
                    deref(rsi(), 8 * (i + 1) as i64),
                    loc
                ))
                .mov(deref(rsi(), 8 * (i + 1) as i64), rax())
                .mov(rax(), loc);
        }
        lambda
            .comment(format!(
                "move the formal parameter '{}' from '{}' into its allocated space ('{}')",
                v,
                rdi(),
                vloc
            ))
            .mov(rdi(), vloc)
            .emit(expr, generator);
        generator.add(lambda.ret());
        self.comment(format!(
            "to construct the closure for '{}', we need to pass the enviroment to the runtime",
            f
        ));
        if fv.len() > 0 {
            self.comment(format!(
                "here we save each free variable in a way that will be understood by a variadic C function"
            ));
        } else {
            self.comment(format!(
                "in this case, there is no environment so we move on"
            ));
        }
        for (i, envv) in fv.iter().enumerate().rev() {
            let loc = self.get(&envv);
            match i {
                0 => self
                    .comment(format!("'{}' ('{}') is moved to '{}'", envv, loc, rdx()))
                    .mov(loc, rdx()),
                1 => self
                    .comment(format!("'{}' ('{}') is moved to '{}'", envv, loc, rcx()))
                    .mov(loc, rcx()),
                2 => self
                    .comment(format!("'{}' ('{}') is moved to '{}'", envv, loc, r8()))
                    .mov(loc, r8()),
                3 => self
                    .comment(format!("'{}' ('{}') is moved to '{}'", envv, loc, r9()))
                    .mov(loc, r9()),
                _ => self
                    .comment(format!("'{}' ('{}') is pushed to the stack", envv, loc))
                    .push(loc),
            };
        }
        let env_len = lambda.get_env().len() - 2;
        self.comment(format!(
            "now we place a pointer to the code for the closure in '{}'",
            rdi()
        ))
        .comment(format!(
            "note how 'leaq' is used relative to the program counter ('{}')",
            rip()
        ))
        .lea(relative(rip(), label), rdi())
        .comment(format!("we also need to let the runtime know how many variables are in our environment (in this case {})", env_len))
        .comment(format!("this information is placed in '{}'", rsi()))
        .mov(constant(env_len as i64), rsi())
        .comment(format!(
            "empty '{}' as the C runtime expects this to be 0",
            rax()
        ))
        .xor(rax(), rax())
        .comment(format!("actually call into the runtime"))
        .call_rt("make_recursive_closure");
        if env_len > 4 {
            let allocated = (env_len - 4) * 8;
            self.comment(format!("while building the closure we pushed {} bytes onto the stack that we no longer need, so we increment '{}' by {}", allocated, rsp(), allocated)).add(constant(allocated as i64), rsp())
        } else {
            self
        }
    }

    fn emit_inl(&mut self, sub: Expr, generator: &mut Generator) -> &mut Code {
        self.comment(format!(
            "compute the value we want to put in the left of the union"
        ))
        .emit(sub, generator)
        .comment(format!(
            "the value we want in the left is left in the accumulator ('{}') so we save this",
            rax()
        ))
        .push(rax())
        .comment(format!(
            "to build the union we need to call into the runtime"
        ))
        .comment(format!(
            "empty '{}' as the C runtime expects this to be 0",
            rax()
        ))
        .xor(rax(), rax())
        .comment(format!("actually call into the runtime"))
        .call_rt("alloc")
        .comment(format!(
            "the heap pointer for the union will be left in the accumulator ('{}')",
            rax()
        ))
        .comment(format!(
            "we restore the computed value into it's new heap location ('{}')",
            deref(rax(), 8)
        ))
        .pop(deref(rax(), 8))
        .comment(format!(
            "we need to mark that we are in the left of a union, so we put a zero into the heap as well"
        ))
        .mov(constant(0), deref(rax(), 0))
    }

    fn emit_inr(&mut self, sub: Expr, generator: &mut Generator) -> &mut Code {
        self.comment(format!(
            "compute the value we want to put in the right of the union"
        ))
        .emit(sub, generator)
        .comment(format!(
            "the value we want in the right is left in the accumulator ('{}') so we save this",
            rax()
        ))
        .push(rax())
        .comment(format!(
            "to build the union we need to call into the runtime"
        ))
        .comment(format!(
            "empty '{}' as the C runtime expects this to be 0",
            rax()
        ))
        .xor(rax(), rax())
        .comment(format!("actually call into the runtime"))
        .call_rt("alloc")
        .comment(format!(
            "the heap pointer for the union will be left in the accumulator ('{}')",
            rax()
        ))
        .comment(format!(
            "we restore the computed value into it's new heap location ('{}')",
            deref(rax(), 8)
        ))
        .pop(deref(rax(), 8))
        .comment(format!(
            "we need to mark that we are in the right of a union, so we put a one into the heap as well"
        ))
        .mov(constant(1), deref(rax(), 0))
    }

    fn emit_case(
        &mut self,
        sub: Expr,
        left: (String, Box<Expr>),
        right: (String, Box<Expr>),
        generator: &mut Generator,
    ) -> &mut Code {
        let inr = Label::new();
        let skip = Label::new();
        self.comment(format!(
            "compute the union that we want to apply the cases to"
        ))
        .emit(sub, generator)
        .comment(format!(
            "the heap pointer to the union is left in the accumulator ('{}')",
            rax()
        ))
        .comment(format!(
            "extract the position (left: 0, right: 1) from the heap and place it in '{}'",
            rbx()
        ))
        .mov(deref(rax(), 0), rbx())
        .comment(format!("test if the union is in left or in right"))
        .cmp(constant(0), rbx())
        .comment(format!("at this point we can move the contents of the union from the heap ('{}') into the accumulator ('{}')", deref(rax(), 8), rax()))
        .mov(deref(rax(), 8), rax())
        .comment(format!("if the union is in right, we jump to '{}'", inr))
        .jne(inr);
        let v_left = self.allocate(left.0.clone());
        self.comment(format!("if we haven't made the jump, move the union value in the accumulator ('{}') into it's allocated location as '{}' ('{}')", rax(), left.0, v_left))
            .comment(format!("continue with the body of the case"))
            .mov(rax(), v_left).emit(*left.1, generator)
            .comment(format!("'{}' goes out of scope here", left.0));
        self.deallocate(left.0);
        self.comment(format!(
            "as we don't want to execute the right branch of the case, we jump over it to '{}'",
            skip
        ))
        .jmp(skip)
        .label(inr);
        let v_right = self.allocate(right.0.clone());
        self.comment(format!("if we have made the jump, we move the union value in the accumulator ('{}') into it's allocated location as '{}' ('{}')", rax(), right.0, v_right))
            .mov(rax(), v_right)
            .comment(format!("continue with the body of the case"))
.emit(*right.1, generator)
            .comment(format!("'{}' goes out of scope here", right.0));
        self.deallocate(right.0);
        self.label(skip)
    }

    fn emit_let(
        &mut self,
        v: String,
        sub: Expr,
        body: Expr,
        generator: &mut Generator,
    ) -> &mut Code {
        let loc = self.allocate(v.clone());
        self.comment(format!(
            "calculate the value for '{}' and leave it in the accumulator ('{}')",
            v,
            rax()
        ))
        .emit(sub, generator)
        .comment(format!(
            "move '{}'s calculated value to its allocated space ('{}')",
            v, loc
        ))
        .mov(rax(), loc)
        .comment(format!("run subsequent computation (body of let)"))
        .emit(body, generator)
        .comment(format!("'{}' goes out of scope here", v));
        self.deallocate(v);
        self
    }

    fn emit_let_fun(
        &mut self,
        f: String,
        lambda: (String, Box<Expr>),
        body: Expr,
        generator: &mut Generator,
    ) -> &mut Code {
        self.comment(format!(
            "produce the closure for '{}' and leave a pointer to it in the accumulator ('{}')",
            f,
            rax()
        ))
        .emit_recursive_lambda(f.clone(), lambda, generator);
        let loc = self.allocate(f.clone());
        self.comment(format!(
            "move '{}'s closure pointer to its allocated space ('{}')",
            f, loc
        ))
        .mov(rax(), loc)
        .comment(format!("run subsequent computation (body of let)"))
        .emit(body, generator)
        .comment(format!("'{}' goes out of scope here", f));
        self.deallocate(f);
        self
    }

    fn emit(&mut self, expr: Expr, generator: &mut Generator) -> &mut Code {
        use Expr::*;
        match expr {
            Int(i) => self
                .comment(format!("move {} into the accumulator ('{}')", i, rax()))
                .mov(constant(i), rax()),
            Bool(b) => self
                .comment(format!(
                    "move the binary encoding of '{}' into the accumulator ('{}')",
                    b,
                    rax()
                ))
                .mov(constant(if b { 1 } else { 0 }), rax()),
            Unit => self
                .comment(format!(
                    "move the binary encoding of '()' into the accumulator ('{}')",
                    rax()
                ))
                .mov(constant(0), rax()),
            What => self
                .comment(format!(
                    "to compute the value of a '?' we need to call the runtime"
                ))
                .comment(format!(
                    "empty '{}' as the C runtime expects this to be 0",
                    rax()
                ))
                .xor(rax(), rax())
                .comment(format!("actually call into the runtime"))
                .call_rt("what"),
            Var(v) => self.emit_var(v),
            UnOp(op, sub) => self.emit_unop(op, *sub, generator),
            BinOp(op, left, right) => self.emit_binop(op, *left, *right, generator),
            If(condition, left, right) => self.emit_if(*condition, *left, *right, generator),
            While(condition, sub) => self.emit_while(*condition, *sub, generator),
            Seq(seq) => self.emit_seq(seq, generator),
            Ref(sub) => self.emit_ref(*sub, generator),
            Deref(sub) => self.emit(*sub, generator).mov(deref(rax(), 0), rax()),
            Fst(sub) => self
                .emit(*sub, generator)
                .comment(format!(
                    "project the first element from the pair and leave it the accumulator ('{}')",
                    rax()
                ))
                .mov(deref(rax(), 0), rax()),
            Snd(sub) => self
                .emit(*sub, generator)
                .comment(format!(
                    "project the second element from the pair and leave it the accumulator ('{}')",
                    rax()
                ))
                .mov(deref(rax(), 8), rax()),
            Pair(left, right) => self.emit_pair(*left, *right, generator),
            Assign(left, right) => self.emit_assign(*left, *right, generator),
            App(left, right) => self.emit_app(*left, *right, generator),
            Lambda(lambda) => self.emit_lambda(lambda, generator),
            Inl(sub) => self.emit_inl(*sub, generator),
            Inr(sub) => self.emit_inr(*sub, generator),
            Case(sub, left, right) => self.emit_case(*sub, left, right, generator),
            Let(v, sub, body) => self.emit_let(v, *sub, *body, generator),
            LetFun(f, lambda, body) => self.emit_let_fun(f, lambda, *body, generator),
        }
    }
}

pub fn generate(expr: Expr) -> String {
    let mut generator = Generator::new();
    let mut entry = Code::new("entry".into());
    let entry = entry.emit(expr, &mut generator);
    generator.add(entry.ret());
    format!("{}", generator)
}
