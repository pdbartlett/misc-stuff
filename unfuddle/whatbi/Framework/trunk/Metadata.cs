/*
 *  Project:        WhaTBI? (What's The Big Idea?)
 *  Filename:       Metadata.cs
 *  Description:    Classes describing type metadata
 */

using System;
using System.Collections;
using System.ComponentModel;
using System.Reflection;
using System.Text;

namespace Pdbartlett.Whatbi
{
    namespace Custom
    {
        public interface IGetTypeInfo
        {
            ITypeInfo GetTypeInfo();
        }
    }
    
    public interface ITypeInfo
    {
        string Name { get; }
        bool IsCollection { get; }
        
        IPropertyInfo[] GetProperties();
        IPropertyInfo GetProperty(string propName);
        object ConvertTo(object o, ITypeInfo dest);
    }
    
    public interface IPropertyInfo
    {
        string Name { get; }
        string DisplayName { get; }
        ITypeInfo TypeInfo { get; }
        
        object GetValue(object o);
    }

    public class TypeToTypeInfoAdapter : ITypeInfo
    {
        private Type m_clrType;
        private PropertyDescriptorCollection m_props;

        public TypeToTypeInfoAdapter(Type clrType)
        {
            if (clrType == null)
                throw new ArgumentNullException();

            m_clrType = clrType;
        }
        
        public Type ClrType { get { return m_clrType; } }
        
        public string Name { get { return m_clrType.Name; } }
        
        public bool IsCollection { get { return m_clrType.GetInterface("ICollection") != null; } }

        public virtual IPropertyInfo[] GetProperties()
        {
            if (m_props == null)
                m_props = TypeDescriptor.GetProperties(m_clrType);
            
            PropDescToPropInfoAdapter[] pi = new PropDescToPropInfoAdapter[m_props.Count];
            
            int i = 0;
            foreach(PropertyDescriptor pd in m_props)
                pi[i++] = new PropDescToPropInfoAdapter(pd);
            
            return pi;
        }
        
        public virtual IPropertyInfo GetProperty(string propName)
        {
            if (m_props == null)
                m_props = TypeDescriptor.GetProperties(m_clrType);
            
            PropertyDescriptor pd = m_props[propName];
            return pd == null ? null : new PropDescToPropInfoAdapter(pd);
        }
        
        public virtual object ConvertTo(object o, ITypeInfo dest)
        {
            // TODO - update to use component model
            
            TypeToTypeInfoAdapter clrDest = dest as TypeToTypeInfoAdapter;
            if (clrDest == null)
                return null;
            
            IConvertible src = o as IConvertible;
            if (src == null)
                return null;
            
            return src.ToType(clrDest.m_clrType, null);
        }
    }

    public class ObjectToTypeInfoAdapter : ITypeInfo
    {
        private object m_obj;
        private PropertyDescriptorCollection m_props;

        public ObjectToTypeInfoAdapter(object obj)
        {
            if (obj == null)
                throw new ArgumentNullException();

            m_obj = obj;
        }
        
        public object Object { get { return m_obj; } }
        
        public string Name { get { return TypeDescriptor.GetClassName(m_obj); } }
        
        public bool IsCollection { get { return m_obj is ICollection; } }

        public virtual IPropertyInfo[] GetProperties()
        {
            if (m_props == null)
                m_props = TypeDescriptor.GetProperties(m_obj, false);
            
            PropDescToPropInfoAdapter[] pi = new PropDescToPropInfoAdapter[m_props.Count];
            
            int i = 0;
            foreach(PropertyDescriptor pd in m_props)
                pi[i++] = new PropDescToPropInfoAdapter(pd);
            
            return pi;
        }
        
        public virtual IPropertyInfo GetProperty(string propName)
        {
            if (m_props == null)
                m_props = TypeDescriptor.GetProperties(m_obj, false);
            
            PropertyDescriptor pd = m_props[propName];
            return pd == null ? null : new PropDescToPropInfoAdapter(pd);
        }
        
        public virtual object ConvertTo(object o, ITypeInfo dest)
        {
            TypeToTypeInfoAdapter adaptThis = new TypeToTypeInfoAdapter(this.GetType());
            return adaptThis.ConvertTo(o, dest);
        }
    }

    public class PropDescToPropInfoAdapter : IPropertyInfo
    {
        private PropertyDescriptor m_propDesc;

        public PropDescToPropInfoAdapter(PropertyDescriptor propDesc)
        {
            if (propDesc == null)
                throw new ArgumentNullException();

            m_propDesc = propDesc;
        }

        public string Name { get { return m_propDesc.Name; } }
        
        public string DisplayName
        {
            get
            {
                // Has type descriptor provided something different to name
                string dispName = m_propDesc.DisplayName;
                string name = Name;
                if (dispName != name)
                    return dispName;
                
                // Otherwise, just "tidy" the name
                
                int c = name.Length;
                if (c == 0)
                    return "";
                
                StringBuilder sb = new StringBuilder(2 * name.Length);
                sb.Append(Char.ToUpper(name[0]));
                for (int i = 1; i < c; ++i)
                {
                    char ch = name[i];
                    if (Char.IsUpper(ch))
                        sb.Append(' ');
                    
                    sb.Append(ch);
                }
                
                return sb.ToString();
            }
        }
        
        public ITypeInfo TypeInfo
        {
            get
            {
                return new TypeToTypeInfoAdapter(m_propDesc.PropertyType);
            }
        }
        
        public override string ToString()
        {
            return DisplayName;
        }
        
        public object GetValue(object o)
        {
            return m_propDesc.GetValue(o);
        }
    }
    
    public abstract class DynamicObjectHelper : ICustomTypeDescriptor
    {
        private PropertyDescriptorCollection m_props;
        
        public void RebuildProperties()
        {
            BuildProps();
        }
        
        protected virtual void BuildProps()
        {
            IPropertyInfo[] pis = InternalGetProperties();
            if (pis != null)
            {
                m_props = new PropertyDescriptorCollection(null);
                foreach (IPropertyInfo pi in pis)
                    m_props.Add(new PropInfoToPropDescAdapter(GetType(), pi));
                    
                return;
            }
            
            m_props = InternalGetPropDescColl();
        }
        
        protected virtual IPropertyInfo[] InternalGetProperties()
        {
            return null;
        }
        
        protected virtual PropertyDescriptorCollection InternalGetPropDescColl()
        {
            return TypeDescriptor.GetProperties(this, true);
        }
        
        public virtual string GetClassName()
        {
            return TypeDescriptor.GetClassName(this, true);
        }
        
        public AttributeCollection GetAttributes()
        {
            return TypeDescriptor.GetAttributes(this, true);
        }
        
        public string GetComponentName()
        {
            return TypeDescriptor.GetComponentName(this, true);
        }
        
        public TypeConverter GetConverter()
        {
            return TypeDescriptor.GetConverter(this, true);
        }
        
        public EventDescriptor GetDefaultEvent()
        {
            return TypeDescriptor.GetDefaultEvent(this, true);
        }
        
        public PropertyDescriptor GetDefaultProperty()
        {
            return TypeDescriptor.GetDefaultProperty(this, true);
        }
        
        public object GetEditor(Type t)
        {
            return TypeDescriptor.GetEditor(this, t, true);
        }
        
        public EventDescriptorCollection GetEvents()
        {
            return TypeDescriptor.GetEvents(this, true);
        }
        
        public EventDescriptorCollection GetEvents(Attribute[] attrs)
        {
            return TypeDescriptor.GetEvents(this, attrs, true);
        }
        
        public PropertyDescriptorCollection GetProperties()
        {
            if (m_props == null)
                BuildProps();
            
            return m_props;
        }
        
        public PropertyDescriptorCollection GetProperties(Attribute[] attrs)
        {
            if (m_props == null)
                BuildProps();
            
            return m_props; // need to filter for .NET v2
        }
        
        public object GetPropertyOwner(PropertyDescriptor prop)
        {
            return this;
        }
    }
    
    public class PropInfoToPropDescAdapter : PropertyDescriptor
    {
        private IPropertyInfo   m_pi;
        private Type            m_clrComponentType;
        private Type            m_clrPropertyType;
        
        public PropInfoToPropDescAdapter(Type clrType, IPropertyInfo pi) : base(pi.Name, null)
        {
            m_pi = pi;
            m_clrComponentType = clrType;
            m_clrPropertyType = GetClrTypeFromTypeInfo(pi.TypeInfo);
        }
        
        private static Type GetClrTypeFromTypeInfo(ITypeInfo ti)
        {
            TypeToTypeInfoAdapter ttia = ti as TypeToTypeInfoAdapter;
            if (ttia != null)
                return ttia.ClrType;

            ObjectToTypeInfoAdapter otia = ti as ObjectToTypeInfoAdapter;
            if (otia != null)
                return otia.Object.GetType();

            throw new ArgumentException();
        }
        
        public override Type ComponentType  { get { return m_clrComponentType; } }
        public override bool IsReadOnly     { get { return true; } }
        public override Type PropertyType   { get { return m_clrPropertyType; } }
        
        public override object GetValue(object o)
        {
            return m_pi.GetValue(o);
        }
        
        public override void SetValue(object o, object value)
        {
            throw new NotImplementedException();
        }
        
        public override bool CanResetValue(object o)
        {
            return false;
        }
        
        public override void ResetValue(object o)
        {
            throw new NotImplementedException();
        }
        
        public override bool ShouldSerializeValue(object o)
        {
            return false;
        }
    }
    
    public delegate object ObjectValueGetter(IPropertyInfo sender, object obj);
        
    public class DynamicPropertyInfo : IPropertyInfo
    {
        private string              m_name;
        private ITypeInfo           m_ti;
        private ObjectValueGetter   m_getter;
        
        public DynamicPropertyInfo(string name, ITypeInfo ti, ObjectValueGetter getter)
        {
            m_name      = name;
            m_ti        = ti;
            m_getter    = getter;
        }
        
        public string Name { get { return m_name; } }
        public string DisplayName { get { return m_name; } }
        public ITypeInfo TypeInfo { get { return m_ti; } }
        
        public object GetValue(object o)
        {
            return m_getter(this, o);
        }
    }
}