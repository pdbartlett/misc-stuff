/*
 *	Project:		WhaTBI? (What's The Big Idea?)
 *	Filename:		DataSource.cs
 *	Description:	Generic datasource interfaces and helper classes
 */

using System;
using System.Collections;
using System.IO;

namespace Pdbartlett.Whatbi
{
    public class SortSpec
    {
        private string  m_propName;
        private bool    m_ascending;
        
        public SortSpec(string propName) : this(propName, true) {}
        
        public SortSpec(string propName, bool ascending)
        {
            m_propName  = propName;
            m_ascending = ascending;
        }
        
        public string PropName  { get { return m_propName;  } }
        public bool   Ascending { get { return m_ascending; } }
        
        public void ToggleAscending()
        {
            m_ascending = !m_ascending;
        }
    }
    
    public class SortDecorator : IDataSource, IDisposable
    {
        private IDataSource m_wrapped;
        private SortSpec    m_sortSpec;
        
        protected IDataSource Wrapped { get { return m_wrapped; } }
        
        public SortDecorator(IDataSource wrapped) : this(wrapped, null) {}
        
        public SortDecorator(IDataSource wrapped, SortSpec sortSpec)
        {
            if (wrapped == null)
                throw new ArgumentNullException();
            
            m_wrapped   = wrapped;
            m_sortSpec  = sortSpec;
        }
        
        public SortSpec SortSpec { get { return m_sortSpec; } set { m_sortSpec = value; } }
        
        public int Count { get { return m_wrapped.Count; } }
        
        public object Get(object key) { return m_wrapped.Get(key); }
        
        public ICollection GetAll() { return Sort(m_wrapped.GetAll()); }
        
        public ICollection Query(IPredicate pred) { return Sort(m_wrapped.Query(pred)); }
        
        private ICollection Sort(ICollection unsorted)
        {
            if (m_sortSpec == null)
                return unsorted;
            
            PropertyComparer comp = new PropertyComparer(m_sortSpec);
            
            return comp.SortCollection(unsorted);
        }
        
        public void Dispose()
        {
            Dispose(true);
        }
        
        ~SortDecorator()
        {
            Dispose(false);
        }
        
        protected virtual void Dispose(bool bDisposing)
        {
            IDisposable wrapDisp = m_wrapped as IDisposable;
            if (wrapDisp != null)
                wrapDisp.Dispose();
            
            if (bDisposing)
                GC.SuppressFinalize(this);
        }
    }
    
    public class UpdatableSortDecorator : SortDecorator, IUpdatableDataSource
    {
        public UpdatableSortDecorator(IUpdatableDataSource wrapped) : base(wrapped) {}
        public UpdatableSortDecorator(IUpdatableDataSource wrapped, SortSpec spec) : base(wrapped, spec) {}
        
        private new IUpdatableDataSource Wrapped { get { return (IUpdatableDataSource)(base.Wrapped); } }
        
		public void Insert(object obj) { Wrapped.Insert(obj); }
		public void Update(object obj) { Wrapped.Update(obj); }
		public void Delete(object key) { Wrapped.Delete(key); }
    }
    
    public class PropertyComparer : IComparer
    {
        SortSpec m_spec;
        
        public PropertyComparer(SortSpec spec)
        {
            if (spec == null)
                throw new ArgumentNullException();
            
            m_spec = spec;
        }
        
        public int Compare(object lhs, object rhs)
        {
            IExpression prop = new Expression.GetProperty(m_spec.PropName);
            object leftField = prop.Evaluate(lhs);
            object rightField = prop.Evaluate(rhs);

            IComparable leftCmp = leftField as IComparable;
            if (leftCmp == null)
            {
                // If not comparable, convert both to strings
                leftCmp = leftField.ToString();
                rightField = rightField.ToString();
            }
            
            int compareFields = leftCmp.CompareTo(rightField);
            return m_spec.Ascending ? compareFields : -compareFields;
        }
        
        public ICollection SortCollection(ICollection unsorted)
        {
            ArrayList work = unsorted as ArrayList;
            if (work == null)
                work = new ArrayList(unsorted);
            
            work.Sort(this);
            
            return work;
        }
    }
}
