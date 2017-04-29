/*
 *	Project:		WhaTBI? (What's The Big Idea?)
 *	Filename:		Tests/SqlQuery.cs
 *	Description:	Test of coverting queries to SQL
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
	[TestFixture] public class SqlQueryTest
	{
		[Test] public void LiteralString()
		{
			IExpression expr = Expr.Literal("Test");
			ISqlConvertible sqlExpr = (ISqlConvertible)expr;
			Assert.AreEqual(sqlExpr.ConvertToSql(null), "'Test'");
		}

		[Test] public void LiteralStringWithQuote()
		{
			IExpression expr = Expr.Literal("Fred's");
			ISqlConvertible sqlExpr = (ISqlConvertible)expr;
			Assert.AreEqual(sqlExpr.ConvertToSql(null), "'Fred''s'");
		}

		[Test] public void LiteralNumber()
		{
			IExpression expr = Expr.Literal(3.142);
			ISqlConvertible sqlExpr = (ISqlConvertible)expr;
			Assert.AreEqual(sqlExpr.ConvertToSql(null), "3.142");
		}

		[Test] public void LiteralDateTime()
		{
			DateTime now = DateTime.Now;
			IExpression expr = Expr.Literal(now);
			ISqlConvertible sqlExpr = (ISqlConvertible)expr;
			Assert.AreEqual(sqlExpr.ConvertToSql(null), "'" + now.ToString("u") + "'");
		}

		[Test] public void GetProperty()
		{
			IORMapping mapper = new CavemanMapping(
				DatabaseDataSourceHelper.GetFactory(DatabaseType.MsSqlServer, "(localhost)", "WhatbiTest"));
			IExpression expr = Expr.GetProperty("Name");;
			ISqlConvertible sqlExpr = (ISqlConvertible)expr;
			Assert.AreEqual(sqlExpr.ConvertToSql(mapper), "Name");
		}

		[Test] public void Key()
		{
			IORMapping mapper = new CavemanMapping(
				DatabaseDataSourceHelper.GetFactory(DatabaseType.MsSqlServer, "(localhost)", "WhatbiTest"));
			IExpression expr = Expr.Key();
			ISqlConvertible sqlExpr = (ISqlConvertible)expr;
			Assert.AreEqual(sqlExpr.ConvertToSql(mapper), "Name");
		}

		[Test] public void Equal()
		{
			IPredicate pred = Expr.Equal(Expr.Literal(1), Expr.Literal(2));
			ISqlConvertible sqlPred = (ISqlConvertible)pred;
			Assert.AreEqual(sqlPred.ConvertToSql(null), "1 = 2");
		}

		[Test] public void NotEqual()
		{
			IPredicate pred = Expr.NotEqual(Expr.Literal(1), Expr.Literal(2));
			ISqlConvertible sqlPred = (ISqlConvertible)pred;
			Assert.AreEqual(sqlPred.ConvertToSql(null), "1 <> 2");
		}

		[Test] public void LessThan()
		{
			IPredicate pred = Expr.LessThan(Expr.Literal(1), Expr.Literal(2));
			ISqlConvertible sqlPred = (ISqlConvertible)pred;
			Assert.AreEqual(sqlPred.ConvertToSql(null), "1 < 2");
		}

		[Test] public void LessThanOrEqual()
		{
			IPredicate pred = Expr.LessThanOrEqual(Expr.Literal(1), Expr.Literal(2));
			ISqlConvertible sqlPred = (ISqlConvertible)pred;
			Assert.AreEqual(sqlPred.ConvertToSql(null), "1 <= 2");
		}

		[Test] public void GreaterThan()
		{
			IPredicate pred = Expr.GreaterThan(Expr.Literal(1), Expr.Literal(2));
			ISqlConvertible sqlPred = (ISqlConvertible)pred;
			Assert.AreEqual(sqlPred.ConvertToSql(null), "1 > 2");
		}

		[Test] public void GreaterThanOrEqual()
		{
			IPredicate pred = Expr.GreaterThanOrEqual(Expr.Literal(1), Expr.Literal(2));
			ISqlConvertible sqlPred = (ISqlConvertible)pred;
			Assert.AreEqual(sqlPred.ConvertToSql(null), "1 >= 2");
		}

		[Test] public void And()
		{
			IPredicate pred = Expr.And(Expr.Equal(Expr.Literal(1), Expr.Literal(2)), Expr.Equal(Expr.Literal(1), Expr.Literal(2)));
			ISqlConvertible sqlPred = (ISqlConvertible)pred;
			Assert.AreEqual(sqlPred.ConvertToSql(null), "(1 = 2) AND (1 = 2)");
		}

		[Test] public void Or()
		{
			IPredicate pred = Expr.Or(Expr.Equal(Expr.Literal(1), Expr.Literal(2)), Expr.Equal(Expr.Literal(1), Expr.Literal(2)));
			ISqlConvertible sqlPred = (ISqlConvertible)pred;
			Assert.AreEqual(sqlPred.ConvertToSql(null), "(1 = 2) OR (1 = 2)");
		}

		[Test] public void Not()
		{
			IPredicate pred = Expr.Not(Expr.Equal(Expr.Literal(1), Expr.Literal(2)));
			ISqlConvertible sqlPred = (ISqlConvertible)pred;
			Assert.AreEqual(sqlPred.ConvertToSql(null), "NOT (1 = 2)");
		}
	}
}
