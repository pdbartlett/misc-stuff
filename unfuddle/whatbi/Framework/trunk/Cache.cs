/*
 *	Project:		WhaTBI? (What's The Big Idea?)
 *	Filename:		Cache.cs
 *	Description:	Caching adaptors/decorators
 */

using System;
using System.Collections;
using System.IO;

namespace Pdbartlett.Whatbi
{
	public interface ICachePolicy
	{
		bool IsUpToDate(object key);
		bool IsInterimWritebackRequired { get; }
		void CacheRefreshed();
	}
	
#if FX_V20
	public static class CachePolicy
#else
	public class CachePolicy
#endif
	{
		public static ICachePolicy NoCache		{ get { return new SimpleCachePolicy(false, true); } }
		public static ICachePolicy Exclusive	{ get { return new SimpleCachePolicy(true, false); } }
		
		public static ICachePolicy ForFile(IFileBased ds)
		{
			if (ds == null)
				throw new ArgumentNullException();
			
			if (ds.Sharing == FileShare.None)
				return Exclusive;
			else
				return new FileTimeCachePolicy(ds.Path, ds.Sharing);
		}
	}
	
	public class SimpleCachePolicy : ICachePolicy
	{
		private bool m_isUpToDate;
		private bool m_isInterimWritebackRequired;

		public SimpleCachePolicy(bool isUpToDate, bool isInterimWritebackRequired)
		{
			m_isUpToDate = isUpToDate;
			m_isInterimWritebackRequired = isInterimWritebackRequired;
		}

		public bool IsUpToDate(object key)		{ return m_isUpToDate; }
		public bool IsInterimWritebackRequired	{ get { return m_isInterimWritebackRequired; } }
		
		public void CacheRefreshed() {}
	}

	public class FileTimeCachePolicy : ICachePolicy
	{
		private string		m_path;
		private bool		m_isInterimWritebackRequired;
		private DateTime	m_cacheRefreshedUtc = DateTime.MinValue;

		public FileTimeCachePolicy(string path, FileShare sharing)
		{
			m_path = path;
			m_isInterimWritebackRequired = (sharing == FileShare.Read || sharing == FileShare.ReadWrite);
		}

		public bool IsUpToDate(object key)
		{
			return m_cacheRefreshedUtc > (new FileInfo(m_path)).LastWriteTimeUtc;
		}

		public bool IsInterimWritebackRequired
		{
			get { return m_isInterimWritebackRequired; }
		}

		public void CacheRefreshed()
		{
			m_cacheRefreshedUtc = DateTime.UtcNow;
		}
	}
	
	public class CachingBulkAdapter : DataSourceHelper, IUpdatableDataSource, IDisposable
	{
		private IBulkDataSource			m_ds;
		private IUpdatableDataSource	m_cache;
		private ICachePolicy			m_policy;
		
		public CachingBulkAdapter(IBulkDataSource ds, ICachePolicy policy)
		{
			m_ds = ds;
			m_policy = policy;
			
			FillCache();
		}
		
		protected override ICollection InternalGetAll(out bool IsCopy)
		{
			IsCopy = true; // map will ensure copies returned
			return m_cache.GetAll();
		}
		
		public override object Get(object key)
		{
			if (!m_policy.IsUpToDate(key))
			{
				m_policy.CacheRefreshed(); // be pessimistic about refresh time
				FillCache();
			}
			
			return m_cache.Get(key);
		}
		
		public void Insert(object obj)
		{
			m_cache.Insert(obj);
			Writeback(false);
		}
		
		public void Update(object obj)
		{
			m_cache.Update(obj);
			Writeback(false);
		}
		
		public void Delete(object key)
		{
			m_cache.Delete(key);
			Writeback(false);
		}
		
		private void FillCache()
		{
			m_cache = new UpdatableMap(m_ds.GetAll());
		}
		
		private void Writeback(bool Force)
		{
			if (Force || m_policy.IsInterimWritebackRequired)
				m_ds.UpdateAll(m_cache.GetAll());
		}
		
		public void Dispose()
		{
			Dispose(true);
		}
		
		private void Dispose(bool IsDisposing)
		{
			if (m_ds != null && m_policy != null)
				Writeback(true);

			IDisposable disp = m_ds as IDisposable;
			if (disp != null)
				disp.Dispose();
			
			if (IsDisposing)
			{
				GC.SuppressFinalize(this);
			}
			// TODO: else raise warning
		}
		
		~CachingBulkAdapter()
		{
			Dispose(false);
		}
	}
	
	public class CachingBulkFileAdapter : CachingBulkAdapter
	{
		public CachingBulkFileAdapter(IFileBulkDataSource ds) : base(ds, CachePolicy.ForFile(ds)) {}
	}
}
