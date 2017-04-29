/*
 *  Project:        Membership Log (application using WhaTBI?)
 *  Filename:       Settings.cs
 *  Description:    Applications settings object
 */

using System;
using System.Collections;
using System.IO;
using System.Runtime.Serialization.Formatters.Soap;
using System.Windows.Forms;

using Pdbartlett.Whatbi;
using Pdbartlett.Whatbi.Custom;

namespace Pdbartlett.MembershipLog
{
    [Serializable]
    public class DetailType : IGetKey, ICloneable
    {
        private string m_name = "";
        
        public string Name
        {
            get { return m_name; }
            set { m_name = value; }
        }
        
        public DetailType() {}
        
        public DetailType(string name)
        {
            m_name = name;
        }
        
        public override string ToString()
        {
            return Name;
        }
        
        public object GetKey()
        {
            return Name;
        }
        
        public object Clone()
        {
            return MemberwiseClone();
        }
    }
    
    [Serializable]
    public class DetailTypeCollection : ListAndUpdatableDataSource
    {
        public DetailTypeCollection() : this(null) {}
        public DetailTypeCollection(ICollection coll) : base(new UpdatableMap(coll)) {}
        
        public override int Add(object o)
        {
            if (o == null)
                throw new ArgumentNullException();
            
            if (!(o is DetailType))
                throw new ArgumentException();
            
            return base.Add(o);
        }
    }
    
    [Serializable]
    public class MembershipType : IGetKey, ICloneable
    {
        private string name = "";
        private Decimal cost;
        
        public string Name
        {
            get { return name; }
            set { name = value; }
        }
        
        public Decimal Cost
        {
            get { return cost; }
            set { cost = value; }
        }
        
        public override string ToString()
        {
            return Name;
        }
        
        public object GetKey()
        {
            return Name;
        }
        
        public object Clone()
        {
            return MemberwiseClone();
        }
    }
    
    [Serializable]
    public class MembershipTypeCollection : ListAndUpdatableDataSource
    {
        public MembershipTypeCollection() : this(null) {}
        public MembershipTypeCollection(ICollection coll) : base(new UpdatableMap(coll)) {}
        
        public override int Add(object o)
        {
            if (o == null)
                throw new ArgumentNullException();
            
            if (!(o is MembershipType))
                throw new ArgumentException();
            
            return base.Add(o);
        }
    }
    
    [Serializable]
    public class ColumnInfo : IGetKey, ICloneable
    {
        private string propertyName;
        private string headerText;
        private int ordinal;
        private int width;
        
        public string PropertyName
        {
            get { return propertyName; }
            set { propertyName = value; }
        }
        
        public string HeaderText
        {
            get { return headerText; }
            set { headerText = value; }
        }
        
        public int Ordinal
        {
            get { return ordinal; }
            set { ordinal = value; }
        }
        
        public int Width
        {
            get { return width; }
            set { width = value; }
        }
        
        public object GetKey()
        {
            return Ordinal;
        }
        
        public object Clone()
        {
            return MemberwiseClone();
        }
    }
    
    [Serializable]
    public class ColumnInfoCollection : ListAndUpdatableDataSource
    {
        public ColumnInfoCollection() : this(null) {}
        public ColumnInfoCollection(ICollection coll) : base(new UpdatableMap(coll)) {}
        
        public override int Add(object o)
        {
            if (o == null)
                throw new ArgumentNullException();
            
            if (!(o is ColumnInfo))
                throw new ArgumentException();
            
            return base.Add(o);
        }
        
        public static ColumnInfoCollection FromTypeInfo(ITypeInfo ti)
        {
            ColumnInfoCollection cols = new ColumnInfoCollection();
            
            int i = 0;
            
            foreach (IPropertyInfo pi in ti.GetProperties())
            {
                if (pi.TypeInfo.IsCollection)
                    continue;
                
                ColumnInfo col = new ColumnInfo();
                col.Ordinal = i++;
                col.PropertyName = pi.Name;
                col.HeaderText = pi.DisplayName;
                col.Width = 0;
                
                cols.Add(col);
            }
            
            return cols;
        }
        
        public static ColumnInfoCollection FromGrid(DataGrid grid)
        {
            if (grid.TableStyles.Count != 1)
                throw new ArgumentException();
            
            DataGridTableStyle table = grid.TableStyles[0];
            ColumnInfoCollection cols = new ColumnInfoCollection();
            
            int i = 0;
            
            foreach (DataGridColumnStyle style in table.GridColumnStyles)
            {
                ColumnInfo col = new ColumnInfo();
                col.Ordinal = i++;
                col.PropertyName = style.MappingName;
                col.HeaderText = style.HeaderText;
                col.Width = style.Width;
                
                cols.Add(col);
            }
            
            return cols;
        }
        
        public void ApplyToGrid(DataGrid grid)
        {
            grid.TableStyles.Clear();
            
            DataGridTableStyle table = new DataGridTableStyle();
            table.MappingName = "ArrayList";
            table.GridColumnStyles.Clear();
            
            int c = Count;
            
            for (int i = 0; i < c; ++i)
            {
                ColumnInfo ci = (ColumnInfo)Get(i);
                DataGridColumnStyle col = new DataGridTextBoxColumn();
                col.MappingName = ci.PropertyName;
                col.HeaderText = ci.HeaderText;
                if (ci.Width > 0)
                    col.Width = ci.Width;
                table.GridColumnStyles.Add(col);
            }
            
            grid.TableStyles.Add(table);
        }
    }
    
    [Serializable]
    public class Settings
    {
        private DetailTypeCollection        m_details;
        private MembershipTypeCollection    m_memberships;
        private IDictionary                 m_filters;
        private ColumnInfoCollection        m_columns;
        
        private static Settings theInstance;
        
        // singleton, so private ctor
        private Settings() {}
        
        // WARNING: not thread safe
        public static void CreateNew(ITypeInfo ti)
        {
            if (theInstance != null)
                throw new BaseException("Settings already exist");
            
            theInstance = new Settings();
            
            theInstance.m_details = new DetailTypeCollection();
            theInstance.m_memberships = new MembershipTypeCollection();
            theInstance.m_filters = new Hashtable();
            theInstance.m_columns = ColumnInfoCollection.FromTypeInfo(ti);
        }

        // WARNING: not thread safe
        public static void Load(string path)
        {
            if (theInstance != null)
                throw new BaseException("Settings already exist");
            
            SoapFormatter fmtr = new SoapFormatter();
            using (FileStream fs = new FileStream(path, FileMode.OpenOrCreate))
            {
                theInstance = (Settings)fmtr.Deserialize(fs);
            }
        }
        
        public static void Save(string path, DataGrid grid)
        {
            Current.m_columns = ColumnInfoCollection.FromGrid(grid);
            
            SoapFormatter fmtr = new SoapFormatter();
            using (FileStream fs = new FileStream(path, FileMode.OpenOrCreate))
            {
                fmtr.Serialize(fs, Current);
            }
        }
        
        public static Settings Current { get { return theInstance; } }

        public IDictionary Filters
        {
            get { return m_filters; }
        }
        
        public DetailTypeCollection Details
        {
            get { return m_details; }
        }
        
        public MembershipTypeCollection Memberships
        {
            get { return m_memberships; }
        }
        
        public ColumnInfoCollection Columns
        {
            get { return m_columns; }
        }
    }
}
