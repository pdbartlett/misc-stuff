/*
 *  Project:        WhaTBI? (What's The Big Idea?)
 *  Filename:       Query.cs
 *  Description:    Query functionality
 */

using System;
using System.Reflection;

namespace Pdbartlett.Whatbi
{
    public interface IExpression
    {
        object Evaluate(object o);
    }
    
    public interface IPredicate
    {
        bool Evaluate(object o);
    }
    
    public interface ISqlConvertible
    {
        string ConvertToSql(IORMapping mapper);
    }
    
    [Serializable()] public abstract class PredicateHelper : IPredicate, IExpression
    {
        public abstract bool Evaluate(object o);
        
        object IExpression.Evaluate(object o)
        {
            return Evaluate(o);
        }
    }
    
    namespace Comparison
    {
        [Serializable()] public abstract class EmptyHelper : PredicateHelper
        {
            private IExpression m_value;
            
            public EmptyHelper(IExpression value)
            {
                m_value = value;
            }
            
            protected bool IsEmpty(object o)
            {
                object result = m_value.Evaluate(o);
                return (result == null || result.ToString() == String.Empty);
            }
        }
        
        [Serializable()] public class Empty : EmptyHelper
        {
            public Empty(IExpression value) : base(value) {}
            
            public override bool Evaluate(object o)
            {
                return IsEmpty(o);
            }
        }
        
        [Serializable()] public class NotEmpty : EmptyHelper
        {
            public NotEmpty(IExpression value) : base(value) {}
            
            public override bool Evaluate(object o)
            {
                return !IsEmpty(o);
            }
        }
        
        [Serializable()] public abstract class ComparisonHelper : PredicateHelper
        {
            protected IExpression m_lhs;
            protected IExpression m_rhs;
            
            public ComparisonHelper(IExpression lhs, IExpression rhs)
            {
                m_lhs = lhs;
                m_rhs = rhs;
            }
            
            protected bool EqualsLeftRight(object o)
            {
                return m_lhs.Evaluate(o).Equals(m_rhs.Evaluate(o));
            }
            
            protected int CompareLeftRight(object o)
            {
                return((IComparable)m_lhs.Evaluate(o)).CompareTo(m_rhs.Evaluate(o));
            }
            
            protected string LhsAsString(object o)
            {
                return m_lhs.Evaluate(o).ToString();
            }
            
            protected string RhsAsString(object o)
            {
                return m_rhs.Evaluate(o).ToString();
            }
            
            protected string LhsAsLCaseString(object o)
            {
                return m_lhs.Evaluate(o).ToString().ToLower();
            }
            
            protected string RhsAsLCaseString(object o)
            {
                return m_rhs.Evaluate(o).ToString().ToLower();
            }
        }
            
        [Serializable()] public abstract class SqlComparisonHelper : ComparisonHelper, ISqlConvertible
        {
            public SqlComparisonHelper(IExpression lhs, IExpression rhs) : base(lhs, rhs) {}
            
            public string ConvertToSql(IORMapping mapper)
            {
                ISqlConvertible lhs = (ISqlConvertible)m_lhs;
                ISqlConvertible rhs = (ISqlConvertible)m_rhs;
                return lhs.ConvertToSql(mapper) + " " + GetSqlOperator() + " " + rhs.ConvertToSql(mapper);
            }
            
            protected abstract string GetSqlOperator();
        }
        
        [Serializable()] public class Equal : SqlComparisonHelper
        {
            public Equal(IExpression lhs, IExpression rhs) : base(lhs, rhs) {}

            public override bool Evaluate(object o)
            {
                return EqualsLeftRight(o);
            }
            
            protected override string GetSqlOperator()
            {
                return "=";
            }
        }
        
        [Serializable()] public class NotEqual : SqlComparisonHelper
        {
            public NotEqual(IExpression lhs, IExpression rhs) : base(lhs, rhs) {}
            
            public override bool Evaluate(object o)
            {
                return !EqualsLeftRight(o);
            }
            
            protected override string GetSqlOperator()
            {
                return "<>";
            }
        }
        
        [Serializable()] public class LessThan : SqlComparisonHelper
        {
            public LessThan(IExpression lhs, IExpression rhs) : base(lhs, rhs) {}

            public override bool Evaluate(object o)
            {
                return CompareLeftRight(o) < 0;
            }
            
            protected override string GetSqlOperator()
            {
                return "<";
            }
        }
        
        [Serializable()] public class LessThanOrEqual : SqlComparisonHelper
        {
            public LessThanOrEqual(IExpression lhs, IExpression rhs) : base(lhs, rhs) {}
            
            public override bool Evaluate(object o)
            {
                return CompareLeftRight(o) <= 0;
            }
            
            protected override string GetSqlOperator()
            {
                return "<=";
            }
        }
        
        [Serializable()] public class GreaterThan : SqlComparisonHelper
        {
            public GreaterThan(IExpression lhs, IExpression rhs) : base(lhs, rhs) {}
            
            public override bool Evaluate(object o)
            {
                return CompareLeftRight(o) > 0;
            }
            
            protected override string GetSqlOperator()
            {
                return ">";
            }
        }
        
        [Serializable()] public class GreaterThanOrEqual : SqlComparisonHelper
        {
            public GreaterThanOrEqual(IExpression lhs, IExpression rhs) : base(lhs, rhs) {}
            
            public override bool Evaluate(object o)
            {
                return CompareLeftRight(o) >= 0;
            }
            
            protected override string GetSqlOperator()
            {
                return ">=";
            }
        }
        
        [Serializable()] public class CaseInsensitiveEqual : ComparisonHelper
        {
            public CaseInsensitiveEqual(IExpression lhs, IExpression rhs) : base(lhs, rhs) {}
            
            public override bool Evaluate(object o)
            {
                return LhsAsLCaseString(o) == RhsAsLCaseString(o);
            }
        }
        
        [Serializable()] public class CaseInsensitiveNotEqual : ComparisonHelper
        {
            public CaseInsensitiveNotEqual(IExpression lhs, IExpression rhs) : base(lhs, rhs) {}
            
            public override bool Evaluate(object o)
            {
                return LhsAsLCaseString(o) != RhsAsLCaseString(o);
            }
        }
        
        [Serializable()] public class Contains : ComparisonHelper
        {
            public Contains(IExpression lhs, IExpression rhs) : base(lhs, rhs) {}
            
            public override bool Evaluate(object o)
            {
                return LhsAsLCaseString(o).IndexOf(RhsAsLCaseString(o)) >= 0;
            }
        }
        
        [Serializable()] public class StartsWith : ComparisonHelper
        {
            public StartsWith(IExpression lhs, IExpression rhs) : base(lhs, rhs) {}
            
            public override bool Evaluate(object o)
            {
                return LhsAsLCaseString(o).StartsWith(RhsAsLCaseString(o));
            }
        }
        
        [Serializable()] public class EndsWith : ComparisonHelper
        {
            public EndsWith(IExpression lhs, IExpression rhs) : base(lhs, rhs) {}
            
            public override bool Evaluate(object o)
            {
                return LhsAsLCaseString(o).EndsWith(RhsAsLCaseString(o));
            }
        }
    }
    
    namespace Logical
    {
        [Serializable()] public abstract class BinaryLogicalOperatorHelper : PredicateHelper, ISqlConvertible
        {
            protected IPredicate m_lhs;
            protected IPredicate m_rhs;
            
            public BinaryLogicalOperatorHelper(IPredicate lhs, IPredicate rhs)
            {
                m_lhs = lhs;
                m_rhs = rhs;
            }
            
            public string ConvertToSql(IORMapping mapper)
            {
                ISqlConvertible lhs = (ISqlConvertible)m_lhs;
                ISqlConvertible rhs = (ISqlConvertible)m_rhs;
                return "(" + lhs.ConvertToSql(mapper) + ") " + GetSqlOperator() + " (" + rhs.ConvertToSql(mapper) + ")";
            }
            
            protected abstract string GetSqlOperator();
        }
        
        [Serializable()] public class And : BinaryLogicalOperatorHelper
        {
            public And(IPredicate lhs, IPredicate rhs) : base(lhs, rhs) {}
            
            public override bool Evaluate(object o)
            {
                return m_lhs.Evaluate(o) && m_rhs.Evaluate(o);
            }

            protected override string GetSqlOperator()
            {
                return "AND";
            }
        }

        [Serializable()] public class Or : BinaryLogicalOperatorHelper
        {
            public Or(IPredicate lhs, IPredicate rhs) : base(lhs, rhs) {}
            
            public override bool Evaluate(object o)
            {
                return m_lhs.Evaluate(o) || m_rhs.Evaluate(o);
            }
            
            protected override string GetSqlOperator()
            {
                return "OR";
            }
        }

        [Serializable()] public class Not : PredicateHelper, ISqlConvertible
        {
            private IPredicate m_operand;
            
            public Not(IPredicate operand)
            {
                m_operand = operand;
            }
            
            public override bool Evaluate(object o)
            {
                return !m_operand.Evaluate(o);
            }
            
            public string ConvertToSql(IORMapping mapper)
            {
                ISqlConvertible operand = (ISqlConvertible)m_operand;
                return "NOT (" + operand.ConvertToSql(mapper) + ")";
            }
        }
    }
    
    namespace Expression
    {
        [Serializable()] public class Literal : IExpression, ISqlConvertible
        {
            private object m_value;
            
            public Literal(object literalValue)
            {
                m_value = literalValue;
            }
            
            public object Evaluate(object o)
            {
                return m_value;
            }
            
            public string ConvertToSql(IORMapping mapper)
            {
                if (m_value == null)
                    return "NULL";
                
                Type t = m_value.GetType();
                
                if (t == typeof(string))
                    return "'" + ((string)m_value).Replace("'", "''") + "'";
                
                if
                (
                    t == typeof(double) ||
                    t == typeof(float)  ||
                    t == typeof(long)   || t == typeof(ulong)   ||
                    t == typeof(int)    || t == typeof(uint)    ||
                    t == typeof(short)  || t == typeof(ushort)  ||
                    t == typeof(sbyte)  || t == typeof(byte)
                )
                {
                    return m_value.ToString();
                }
                
                if (t == typeof(DateTime))
                    return "'" + ((DateTime)m_value).ToString("u") + "'";
                
                throw new BaseException("Literal type not convertible to SQL: " + m_value.GetType().FullName);
            }
        }

        [Serializable()] public class GetProperty : IExpression, ISqlConvertible
        {
            private string m_propertyName;
            
            public GetProperty(string propertyName)
            {
                m_propertyName = propertyName;
            }
            
            public object Evaluate(object o)
            {
                ITypeInfo ti = Utility.GetTypeInfo(o);
                IPropertyInfo pi = ti.GetProperty(m_propertyName);
                
                if (pi == null)
                    throw new BaseException("Property '" + m_propertyName + "' not found for type '" + ti.Name + "'");
                
                return pi.GetValue(o);
            }
            
            public string ConvertToSql(IORMapping mapper)
            {
                return mapper.GetColumnForProperty(m_propertyName);
            }
        }
        
        [Serializable()] public class Self : IExpression
        {
            public object Evaluate(object o)
            {
                return o;
            }
        }

        [Serializable()] public class Key : IExpression, ISqlConvertible
        {
            public object Evaluate(object o)
            {
                return Utility.GetKey(o);
            }

            public string ConvertToSql(IORMapping mapper)
            {
                return mapper.GetKeyColumn();
            }
        }
    }
}
