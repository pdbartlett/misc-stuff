/*
 *  Project:        WhaTBI? (What's The Big Idea?)
 *  Filename:       Utility.cs
 *  Description:    Tests for various utilities
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
    [TestFixture] public class UtilityTest : BaseTest
    {
        private int     TestInt     = 42;
        private string  TestString  = "Hello world!";
        private Caveman TestObject   = new Caveman("Fred", "Quarryman"); 
        
        [Test] public void GetReadOnlyCopyOfInt()
        {
            GetReadOnlyHelper(TestInt, false);      // as value type
        }
        
        [Test] public void GetReadOnlyCopyOfString()
        {
            GetReadOnlyHelper(TestString, false);   // as immutable, string class "clone" doesn't create new copy
        }
        
        [Test] public void GetReadOnlyCopyOfObject()
        {
            GetReadOnlyHelper(TestObject, true);
        }
        
        [Test] public void GetReadOnlyCopyOfCollection()
        {
            GetReadOnlyHelper(TestData, true);
        }
        
        [Test, ExpectedException(typeof(BaseException))] public void GetKeyForInt()
        {
            Utility.GetKey(TestInt);
        }
        
        [Test, ExpectedException(typeof(BaseException))] public void GetKeyForString()
        {
            Utility.GetKey(TestString);
        }
        
        [Test] public void GetKeyForObject()
        {
            Assert.AreEqual(TestObject.Name, Utility.GetKey(TestObject));
        }
        
        [Test] public void PredicateHelper()
        {
            IExpression expr = new Comparison.Equal(Expr.Literal(1), Expr.Literal(1));
            Assert.AreEqual(expr.Evaluate(null), true);
        }

        [Test] public void GetProperties()
        {
            IPropertyInfo[] pi = Utility.GetProperties(TestObject);
            Assert.AreEqual(pi.Length, 2);
            Assert.IsFalse(pi[0].Name == pi[1].Name);
            Assert.IsTrue(pi[0].Name == "Name" || pi[0].Name == "Job");
            Assert.IsTrue(pi[1].Name == "Name" || pi[1].Name == "Job");
        }
        
        private void GetReadOnlyHelper(object ObjectToTest, bool DoRefCheck)
        {
            object CopyObject = Utility.GetReadOnly(ObjectToTest);
            
            ICollection testColl = ObjectToTest as ICollection;
            if (testColl == null)
            {
                ReadOnlyCheckItem(ObjectToTest, CopyObject, DoRefCheck);
            }
            else
            {
                ICollection copyColl = CopyObject as ICollection;
                
                Assert.IsNotNull(copyColl);
                Assert.AreEqual(testColl.Count, copyColl.Count);
                
                IList srcList = Utility.MakeList(testColl);
                IList dstList = Utility.MakeList(copyColl);
                
                for (int i = 0; i < srcList.Count; ++i)
                {
                    ReadOnlyCheckItem(srcList[i], dstList[i], DoRefCheck);
                }
            }
        }
        
        private void ReadOnlyCheckItem(object test, object copy, bool DoRefCheck)
        {
            Assert.IsTrue(Object.Equals(copy, test), "Copied object is not equivalent");
            if (DoRefCheck)
                Assert.IsFalse(Object.ReferenceEquals(copy, test), "Copy is not a distinct object");
        }
    }
}
