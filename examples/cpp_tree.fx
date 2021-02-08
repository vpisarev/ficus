// C++ compling example
// Run it with -c++ option, e.g.:
//
// ficus -run -O3 ../examples/cpp_tree.fx -c++
pragma "c++"

ccode //Imitation of very big included third-party library.
{
    #ifndef __cplusplus
    #error "recompile this example with -c++ option passed to Ficus"
    #endif

    #include <typeinfo>
    #include <set>
    #include <string>
    #include <iostream>
    #include <sstream>
    #include <memory>

    class Expression;
    class Number;
    class Variable;

    class Expression
    {
    friend Expression operator+(const Expression& arg1, const Expression& arg2);
    friend Expression operator*(const Expression& arg1, const Expression& arg2);
    friend Expression operator-(const Expression& arg1, const Expression& arg2);
    private:
        enum typeOfAction {
            ePlus,
            eMult,
            eBinaryMinus
        } actType;
        std::shared_ptr<Expression> arg1, arg2;
    protected:
        Expression() {};
        virtual std::shared_ptr<Expression> clone() const
        {
            return std::shared_ptr<Expression>(new Expression(arg1, arg2, actType));
        }
        std::string printtype() const { return typeid(*this).name();};
        virtual std::string _printTree() const
        {
            std::string result = arg1->printtype() + std::string("{") + arg1->_printTree() + std::string("}|");
            result += (actType==ePlus ? std::string("+") : (actType==eMult ? std::string("*") : std::string("-"))) + std::string("|");
            result += arg2->printtype() + std::string("{") + arg2->_printTree() + std::string("}");
            return result;
        }
    private:
        Expression(const Expression& a_arg1, const Expression& a_arg2, typeOfAction a_actType):
            arg1(a_arg1.clone()),
            arg2(a_arg2.clone()),
            actType(a_actType) {}
        Expression(std::shared_ptr<Expression> a_arg1, std::shared_ptr<Expression> a_arg2, typeOfAction a_actType):
            arg1(a_arg1->clone()),
            arg2(a_arg2->clone()),
            actType(a_actType) {}
    public:
        virtual std::string printTree() const
        {
            return printtype() + std::string("{") + _printTree() + std::string("}");
        }
    };

    Expression operator+(const Expression& arg1, const Expression& arg2)
    {
        return Expression(arg1,arg2, Expression::ePlus);
    }

    Expression operator*(const Expression& arg1, const Expression& arg2)
    {
        return Expression(arg1,arg2, Expression::eMult);
    }

    Expression operator-(const Expression& arg1, const Expression& arg2)
    {
        return Expression(arg1,arg2, Expression::eBinaryMinus);
    }


    class Number: public Expression
    {
    private:
        double value;
    public:
        Number(double a_value): value(a_value) {}
    protected:
        virtual std::string _printTree() const
        {
            std::stringstream o;
              if (!(o << value))
                return "";
            return o.str();
        }
        virtual std::shared_ptr<Expression> clone() const
        {
            return std::make_shared<Number>(value);
        }
    };


    class Variable: public Expression
    {
    private:
        std::string name;
    public:
        Variable(const std::string a_name): name(a_name) {}
    protected:
        virtual std::shared_ptr<Expression> clone() const
        {
            return std::make_shared<Variable>(name);
        }
        virtual std::string _printTree() const
        {
            return name;
        }
    };


    // We need custom free function for C++ objects
    // because they are created with new.
    // Usually ficus use malloc.

    void expression_free(void* ptr)
    {
        delete static_cast<Expression*>(ptr);
    }

}

// Ficus interface for declared C++ terms
type eExpr_t = { handle: cptr }

fun create_eExpr(varname: string): eExpr_t = ccode
{
    fx_cstr_t cvarname;
    int stat = fx_str2cstr(varname, &cvarname, 0, 0);
    if(stat>=0)
    {
        Variable* toWrap = new Variable(cvarname.data);
        fx_make_cptr(toWrap, expression_free, &fx_result->handle);
    }
    return stat;
}

fun create_eExpr(value: double): eExpr_t = ccode
{
    Number* toWrap = new Number(value);
    fx_make_cptr(toWrap, expression_free, &fx_result->handle);
    return 0;
}

fun to_string(expression: eExpr_t): string = ccode
{
    Expression* cpp_expr = (Expression*)(expression->handle->ptr);
    std::string cstr = cpp_expr->printTree();
    return fx_cstr2str(cstr.c_str(), cstr.size(), fx_result);
}

operator +(exp1: eExpr_t, exp2: eExpr_t): eExpr_t = ccode
{
    Expression& cpp_expr1 = *((Expression*)(exp1->handle->ptr));
    Expression& cpp_expr2 = *((Expression*)(exp2->handle->ptr));
    Expression* toWrap = new Expression(cpp_expr1 + cpp_expr2);
    fx_make_cptr(toWrap, expression_free, &fx_result->handle);
    return 0;
}

operator -(exp1: eExpr_t, exp2: eExpr_t): eExpr_t = ccode
{
    Expression& cpp_expr1 = *((Expression*)(exp1->handle->ptr));
    Expression& cpp_expr2 = *((Expression*)(exp2->handle->ptr));
    Expression* toWrap = new Expression(cpp_expr1 - cpp_expr2);
    fx_make_cptr(toWrap, expression_free, &fx_result->handle);
    return 0;
}

operator *(exp1: eExpr_t, exp2: eExpr_t): eExpr_t = ccode
{
    Expression& cpp_expr1 = *((Expression*)(exp1->handle->ptr));
    Expression& cpp_expr2 = *((Expression*)(exp2->handle->ptr));
    Expression* toWrap = new Expression(cpp_expr1 * cpp_expr2);
    fx_make_cptr(toWrap, expression_free, &fx_result->handle);
    return 0;
}

fun cpp_tree_example(): void = ccode
{
    Variable x("x");
    Number five(5);
    Expression expr = (x+five)*(x-five);
    std::cout<<five.printTree()<<std::endl;
    std::cout<<expr.printTree()<<std::endl;
    return 0;
}

// Action

println("==== C++ prints tree: ====")
cpp_tree_example()

println("==== Tree prints C++: ====")
val x = create_eExpr("x")
val five = create_eExpr(5.0)
val expression = (x+five)*(x-five)
println(to_string(five))
println(to_string(expression))