/*
 *  Project:        WhaTBI? (What's The Big Idea?)
 *  Filename:       ExpressionFactory.cs
 *  Description:    Helper class for construction expressions
 */

using System;

namespace Pdbartlett.Whatbi
{
    public delegate IPredicate UnaryPredicateBuilder(IExpression x);
    public delegate IPredicate BinaryPredicateBuilder(IExpression x, IExpression y);
    public delegate IPredicate BinaryTextPredicateBuilder(IExpression x, IExpression y);
    public delegate IPredicate TertiaryPredicateBuilder(IExpression x, IExpression y, IExpression z);
    
    public class PredicateInfo
    {
        private string  m_name;
        private int     m_arity;
        private bool    m_textBased;
        private object  m_builder;

        public PredicateInfo(string name, object builder)
        {
            m_name = name;
            m_builder = builder;
            
            if (builder is UnaryPredicateBuilder)
                m_arity = 1;
            else if (builder is BinaryPredicateBuilder || builder is BinaryTextPredicateBuilder)
                m_arity = 2;
            else if (builder is TertiaryPredicateBuilder)
                m_arity = 3;
            else
                throw new ArgumentException();
            
            m_textBased = (builder is BinaryTextPredicateBuilder);
        }

        public string Name { get { return m_name; } }
        
        public int Arity { get { return m_arity; } }

        public override string ToString()
        {
            return Name;
        }
        
        public IPredicate BuildPropertyPredicate(IPropertyInfo pi, params string[] stringValues)
        {
            if (stringValues.Length + 1 != m_arity)
                throw new ArgumentException();
            
            IExpression[] exprs = new IExpression[m_arity];
            exprs[0] = Expr.GetProperty(pi.Name);
            
            for (int i = 0; i < stringValues.Length; ++i)
            {
                if (m_textBased)
                    exprs[i+1] = Expr.Literal(stringValues[i]);
                else
                    exprs[i+1] = Expr.Literal(GetCorrectTypedValue(pi.TypeInfo, stringValues[i]));
                
                if (exprs[i+1] == null)
                    throw new ArgumentException();
            }
            
            return BuildPredicate(exprs);
        }
        
        public IPredicate BuildPredicate(params IExpression[] exprs)
        {
            if (exprs.Length  != m_arity)
                throw new ArgumentException();
            
            UnaryPredicateBuilder upb = m_builder as UnaryPredicateBuilder;
            if (upb != null)
                return upb(exprs[0]);
            
            BinaryPredicateBuilder bpb = m_builder as BinaryPredicateBuilder;
            if (bpb != null)
                return bpb(exprs[0], exprs[1]);
            
            BinaryTextPredicateBuilder btpb = m_builder as BinaryTextPredicateBuilder;
            if (btpb != null)
                return btpb(exprs[0], exprs[1]);

            TertiaryPredicateBuilder tpb = m_builder as TertiaryPredicateBuilder;
            if (tpb != null)
                return tpb(exprs[0], exprs[1], exprs[2]);

            throw new ArgumentException(); // TODO not quite right, but it will do for now...
        }

		private object GetCorrectTypedValue(ITypeInfo ti, string text)
		{
			object typedValue = GetTypedValue(text);
			return Utility.ConvertTo(typedValue, ti);
		}

		private object GetTypedValue(string text)
		{
			int i;
			if (Utility.TryParseInt32(text, out i))
				return i;

			double d;
			if (Utility.TryParseDouble(text, out d))
				return d;
			
			DateTime dt;
			if (Utility.TryParseDateTime(text, out dt))
				return dt;

			return text;
		}
    }

#if FX_V20
    public static class Expr
#else
    public class Expr
#endif
    {
        // comparison predicates

        private static PredicateInfo[] s_predicates = new PredicateInfo[]
        {
            new PredicateInfo("is empty", new UnaryPredicateBuilder(Empty))
        ,   new PredicateInfo("is not empty", new UnaryPredicateBuilder(NotEmpty))
        ,   new PredicateInfo("equal to", new BinaryPredicateBuilder(Equal))
        ,   new PredicateInfo("equal to (case insensitive)", new BinaryTextPredicateBuilder(CaseInsensitiveEqual))
        ,   new PredicateInfo("not equal to", new BinaryPredicateBuilder(NotEqual))
        ,   new PredicateInfo("not equal to (case insensitive)", new BinaryTextPredicateBuilder(CaseInsensitiveNotEqual))
        ,   new PredicateInfo("less than", new BinaryPredicateBuilder(LessThan))
        ,   new PredicateInfo("less than or equal to", new BinaryPredicateBuilder(LessThanOrEqual))
        ,   new PredicateInfo("greater than", new BinaryPredicateBuilder(GreaterThan))
        ,   new PredicateInfo("greater than or equal to", new BinaryPredicateBuilder(GreaterThanOrEqual))
        ,   new PredicateInfo("starts with", new BinaryTextPredicateBuilder(StartsWith))
        ,   new PredicateInfo("ends with", new BinaryTextPredicateBuilder(EndsWith))
        ,   new PredicateInfo("contains", new BinaryTextPredicateBuilder(Contains))
        };

        public static PredicateInfo[] GetPredicateInfo()
        {
            return s_predicates; 
        }
        
        public static IPredicate Empty(IExpression value)
        {
            return new Comparison.Empty(value);
        }
        
        public static IPredicate NotEmpty(IExpression value)
        {
            return new Comparison.NotEmpty(value);
        }
        
        public static IPredicate Equal(IExpression lhs, IExpression rhs)
        {
            return new Comparison.Equal(lhs, rhs);
        }
        
        public static IPredicate NotEqual(IExpression lhs, IExpression rhs)
        {
            return new Comparison.NotEqual(lhs, rhs);
        }
        
        public static IPredicate LessThan(IExpression lhs, IExpression rhs)
        {
            return new Comparison.LessThan(lhs, rhs);
        }
        
        public static IPredicate LessThanOrEqual(IExpression lhs, IExpression rhs)
        {
            return new Comparison.LessThanOrEqual(lhs, rhs);
        }
        
        public static IPredicate GreaterThan(IExpression lhs, IExpression rhs)
        {
            return new Comparison.GreaterThan(lhs, rhs);
        }
        
        public static IPredicate GreaterThanOrEqual(IExpression lhs, IExpression rhs)
        {
            return new Comparison.GreaterThanOrEqual(lhs, rhs);
        }
        
        public static IPredicate CaseInsensitiveEqual(IExpression lhs, IExpression rhs)
        {
            return new Comparison.CaseInsensitiveEqual(lhs, rhs);
        }
        
        public static IPredicate CaseInsensitiveNotEqual(IExpression lhs, IExpression rhs)
        {
            return new Comparison.CaseInsensitiveNotEqual(lhs, rhs);
        }
        
        public static IPredicate Contains(IExpression lhs, IExpression rhs)
        {
            return new Comparison.Contains(lhs, rhs);
        }
        
        public static IPredicate StartsWith(IExpression lhs, IExpression rhs)
        {
            return new Comparison.StartsWith(lhs, rhs);
        }
        
        public static IPredicate EndsWith(IExpression lhs, IExpression rhs)
        {
            return new Comparison.EndsWith(lhs, rhs);
        }
        
        // logical predicates
        
        public static IPredicate And(IPredicate lhs, IPredicate rhs)
        {
            return new Logical.And(lhs, rhs);
        }
        
        public static IPredicate Or(IPredicate lhs, IPredicate rhs)
        {
            return new Logical.Or(lhs, rhs);
        }
        
        public static IPredicate Not(IPredicate operand)
        {
            return new Logical.Not(operand);
        }
        
        // other expressions
        
        public static IExpression Literal(object literalValue)
        {
            return new Expression.Literal(literalValue);
        }
        
        public static IExpression GetProperty(string propertyName)
        {
            return new Expression.GetProperty(propertyName);
        }
        
        public static IExpression Self()
        {
            return new Expression.Self();
        }

        public static IExpression Key()
        {
            return new Expression.Key();
        }
    }
}
