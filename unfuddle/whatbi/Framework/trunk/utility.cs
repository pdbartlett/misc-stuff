/*
 *  Project:        WhaTBI? (What's The Big Idea?)
 *  Filename:       Utility.cs
 *  Description:    Various utilities
 */

using System;
using System.Collections;
using System.Globalization;
using System.IO;
using System.Reflection;
using System.Xml.Serialization;

namespace Pdbartlett.Whatbi
{
    namespace Custom
    {
        public interface IGetReadOnly
        {
            Object GetReadOnly();
        }
        
        public interface IGetKey
        {
            Object GetKey();
        }
    }
    
    public class BaseException : Exception
    {
        public BaseException(string message) : base(message) {}
    }

#if FX_V20  
    public static class Utility
#else
    public class Utility
#endif
    {
        public static Object GetReadOnly(Object o)
        {
            if (o == null)
                return null;
            
            Custom.IGetReadOnly custom = o as Custom.IGetReadOnly; 
            if (custom != null)
                return custom.GetReadOnly();
            
            ICollection coll = o as ICollection;
            if (coll != null)
            {
                ArrayList list = new ArrayList(coll.Count);
                foreach (object item in coll)
                    list.Add(GetReadOnly(item));
                    
                return list;
            }
            
            ICloneable cloneable = o as ICloneable;
            if (cloneable != null)
                return cloneable.Clone();
            
            if (o is ValueType)
                return o;
            
            throw new BaseException("Cannot generate read-only copy of object");
        }
        
        public static Object GetKey(Object o)
        {
            Custom.IGetKey custom = o as Custom.IGetKey;
            if (custom != null)
                return custom.GetKey();
            
            throw new BaseException("Cannot determine key for object");
        }
        
        public static ITypeInfo GetTypeInfo(Type t)
        {
            return new TypeToTypeInfoAdapter(t);
        }

        public static ITypeInfo GetTypeInfo(Object o)
        {
            Custom.IGetTypeInfo custom = o as Custom.IGetTypeInfo;
            if (custom != null)
                return custom.GetTypeInfo();
            
            return new ObjectToTypeInfoAdapter(o);
        }

        public static IPropertyInfo[] GetProperties(Object o)
        {
            ITypeInfo ti = GetTypeInfo(o);
            return ti.GetProperties();
        }
        
        public static object ConvertTo(object o, ITypeInfo dest)
        {
            ITypeInfo src = GetTypeInfo(o);
            return src.ConvertTo(o, dest);
        }
        
        public static bool TryParseDouble(string s, out double d)
        {
            return Double.TryParse(s, NumberStyles.Float, NumberFormatInfo.CurrentInfo, out d);
        }
        
        public static bool TryParseInt32(string s, out int i)
        {
#if FX_V20
            return Int32.TryParse(s, out i);
#else
            double d;
            if (Double.TryParse(s, NumberStyles.Integer, NumberFormatInfo.CurrentInfo, out d))
            {
                try
                {
                    i = Int32.Parse(s);
                    return true;
                }
                catch (Exception) {}
            }
            
            i = 0;
            return false;
#endif
        }
        
        public static bool TryParseDateTime(string s, out DateTime dt)
        {
            // !!! Potential performance issue - exceptions thrown in "mainstream" case !!!
            try
            {
                dt = DateTime.Parse(s);
                return true;
            }
            catch (Exception)
            {
                dt = DateTime.Now;
                return false;
            }
        }

        public static IList MakeList(ICollection coll)
        {
            IList list = coll as IList;
            if (list == null)
                list = new ArrayList(coll);
            
            return list;
        }
        
        public static ArrayList MakeArrayList(ICollection coll)
        {
            ArrayList list = coll as ArrayList;
            if (list == null)
                list = new ArrayList(coll);
            
            return list;
        }
    }

#if FX_V20
    public static class XmlUtility
#else
    public class XmlUtility
#endif
    {
        public static void SerializeObjectToFile(object data, string fileName)
        {
            SerializeObjectToFile(data, data.GetType(), null, fileName);
        }

        public static void SerializeObjectToFile(object data, Type type, string fileName)
        {
            SerializeObjectToFile(data, type, null, fileName);
        }

        public static void SerializeObjectToFile(object data, Type type, Type[] extraTypes, string fileName)
        {
            using (TextWriter writer = new StreamWriter(fileName))
            {
                SerializeObject(data, type, extraTypes, writer);
            }
        }

        public static void SerializeObject(object data, Type type, Type[] extraTypes, TextWriter writer)
        {
            GetXmlSerializer(type, extraTypes).Serialize(writer, data);
        }

        public static object DeserializeObjectFromFile(Type type, string fileName)
        {
            return DeserializeObjectFromFile(type, null, fileName);
        }

        public static object DeserializeObjectFromFile(Type type, Type[] extraTypes, string fileName)
        {
            using (TextReader reader = new StreamReader(fileName))
            {
                return DeserializeObject(type, extraTypes, reader);
            }
        }

        public static object DeserializeObject(Type type, Type[] extraTypes, TextReader reader)
        {
            return GetXmlSerializer(type, extraTypes).Deserialize(reader);
        }
        
        private static XmlSerializer GetXmlSerializer(Type type, Type[] extraTypes)
        {
            if (extraTypes == null)
                return new XmlSerializer(type);

            return new XmlSerializer(type, extraTypes);
        }
    }
}
