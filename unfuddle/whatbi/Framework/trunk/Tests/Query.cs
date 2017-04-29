/*
 *  Project:        WhaTBI? (What's The Big Idea?)
 *  Filename:       Tests/Query.cs
 *  Description:    Tests query-related functionality
 */

using System;
using System.Collections;
using System.Globalization;

#if USE_MBUNIT
using MbUnit.Core.Framework;
using MbUnit.Framework;
#else
using NUnit.Framework;
#endif

using Pdbartlett.Whatbi;
using Pdbartlett.Whatbi.Examples;

namespace Pdbartlett.Whatbi.Tests
{
    [TestFixture] public class QueryTest
    {
        [Test] public void EqualBuilder()
        {
            foreach (PredicateInfo pi in Expr.GetPredicateInfo())
            {
                if (pi.Name == "Equals")
                {
                    IPredicate pred = pi.BuildPredicate(Expr.Literal(1), Expr.Literal(1));
                    Assert.IsTrue(pred.Evaluate(null));
                    break;
                }
            }
        }
        
        [Test] public void GetProperty()
        {
            IExpression expr = Expr.GetProperty("Length");
            Assert.AreEqual(expr.Evaluate("Hello world!"), 12);
        }
        
        // TODO need to add many more tests here...
    }
}
