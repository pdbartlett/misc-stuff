/*
 *	Project:		WhaTBI? (What's The Big Idea?)
 *	Filename:		Pretest\Main.cs
 *	Description:	Entry point for pre-test app (debugging aid)
 */

using System;

using Pdbartlett.Whatbi;
using Pdbartlett.Whatbi.Examples;

namespace Pdbartlett.Whatbi.Pretest
{
    public class EntryPoint
    {
        static void Main(string[] args)
        {
            object o = new CustomTypeDesc();
            ITypeInfo ti = Utility.GetTypeInfo(o);
            
            Console.Out.WriteLine("Name: {0}", ti.Name);
            Console.Out.WriteLine("#Props: {0}", ti.GetProperties().Length);
        }
    }
}
