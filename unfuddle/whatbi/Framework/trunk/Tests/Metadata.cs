/*
 *	Project:		WhaTBI? (What's The Big Idea?)
 *	Filename:		Metadata.cs
 *	Description:	Tests for metadata manipulation
 */

using System;
using System.Collections;

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
	public abstract class DynamicBaseTest : BaseTest
	{
        protected abstract object DynamicObject { get; }
        
        protected ITypeInfo DynTypeInfo { get { return Utility.GetTypeInfo(DynamicObject); } }
        
        [Test] public void TypeName()
        {
            Assert.AreEqual(DynTypeInfo.Name, "UltimateAnswer");
        }
        
        [Test] public void TypeIsCollection()
        {
            Assert.IsFalse(DynTypeInfo.IsCollection);
        }
        
        [Test] public void Properties()
        {
            Assert.AreEqual(DynTypeInfo.GetProperties().Length, 1);
        }
        
        [Test] public void Property()
        {
            IPropertyInfo pi = DynTypeInfo.GetProperty("Value");
            Assert.AreEqual(pi.Name, "Value");
            Assert.AreEqual(pi.GetValue(DynamicObject), 42);
        }
    }
    
    [TestFixture] public class WhatbiDynamicTest : DynamicBaseTest
    {
        protected override object DynamicObject { get { return new Dynamic(); } }
    }
    
    
    [TestFixture] public class CustomTypeDescTest : DynamicBaseTest
    {
        protected override object DynamicObject { get { return new CustomTypeDesc(); } }
    }
}
