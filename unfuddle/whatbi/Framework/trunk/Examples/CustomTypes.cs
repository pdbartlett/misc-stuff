/*
 *	Project:		WhaTBI? (What's The Big Idea?)
 *	Filename:		CustomTypes.cs
 *	Description:	Various custom types, usually for testing or examples
 */

using System;
using System.ComponentModel;

using Pdbartlett.Whatbi;

namespace Pdbartlett.Whatbi.Examples
{
	public enum TestEnum { Red, Green, Blue };
	
	public class Caveman : ICloneable, Custom.IGetKey 
	{
		private string m_name;
		private string m_job;
		
		public Caveman() {}
		
		public Caveman(string name, string job)
		{
			m_name = name;
			m_job = job;
		}
		
		public string Name
		{
			get { return m_name; }
			set { m_name = value; }
		}
		
		public string Job
		{
			get { return m_job; }
			set { m_job = value; }
		}
		
		public object Clone()
		{
			return MemberwiseClone();
		}
		
		public override bool Equals(object otherObject)
		{
			Caveman otherCaveman = otherObject as Caveman;
			return otherCaveman != null && otherCaveman.Name == Name && otherCaveman.Job == Job;
		}
		
		public override int GetHashCode()
		{
			return Name.GetHashCode();
		}
		
		public object GetKey()
		{
			return Name;
		}
	}
    
    public class Dynamic : Custom.IGetTypeInfo, ITypeInfo, IPropertyInfo
    {
        ITypeInfo Custom.IGetTypeInfo.GetTypeInfo()
        {
            return this;
        }
        
        string ITypeInfo.Name { get { return "UltimateAnswer"; } }
        bool ITypeInfo.IsCollection { get { return false; } }
        
        IPropertyInfo[] ITypeInfo.GetProperties()
        {
            return new IPropertyInfo[] { ((ITypeInfo)this).GetProperty("Value") };
        }
        
        IPropertyInfo ITypeInfo.GetProperty(string propName)
        {
            return (propName == "Value") ? this : null;
        }
        
        object ITypeInfo.ConvertTo(object o, ITypeInfo dest)
        {
            throw new NotImplementedException();
        }
        
        string IPropertyInfo.Name { get { return "Value"; } }
        string IPropertyInfo.DisplayName { get { return "Value"; } }
        ITypeInfo IPropertyInfo.TypeInfo { get { return new TypeToTypeInfoAdapter(typeof(int)); } }
        
        object IPropertyInfo.GetValue(object o)
        {
            return 42;
        }
    }
    
    public class CustomTypeDesc : DynamicObjectHelper
    {
        private IPropertyInfo[] m_props;
        
        public override string GetClassName() { return "UltimateAnswer"; }
        
        protected override IPropertyInfo[] InternalGetProperties()
        {
            if (m_props == null)
            {
                m_props = new IPropertyInfo[1];
                m_props[0] = new DynamicPropertyInfo("Value", Utility.GetTypeInfo(typeof(int)),
                                                     new ObjectValueGetter(AnswerGetter));
            }
            
            return m_props;
        }
        
        private object AnswerGetter(IPropertyInfo sender, object o)
        {
            return 42;
        }
    }
}
