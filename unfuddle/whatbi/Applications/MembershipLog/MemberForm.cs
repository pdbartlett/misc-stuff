/*
 *	Project:		Membership Log (application using WhaTBI?)
 *	Filename:		MemberForm.cs
 *	Description:	Form to display, update and create members
 */

using System;
using System.Drawing;
using System.Globalization;
using System.Windows.Forms;

using Pdbartlett.Whatbi;

namespace Pdbartlett.MembershipLog
{
	public class MemberForm : GridForm
	{
		// Constants
		private const int MIN_FORM_WIDTH	= 400;
		private const int MIN_FORM_HEIGHT	= 400;

		// Controls
		private TabControl	m_tabs;
		private DataGrid	m_detailsGrid;
		private DataGrid	m_membershipsGrid;
		private DataGrid	m_paymentsGrid;

		// Helper properties
		private Member ThisMember { get { return ((Member)Subject); } }
		
		// Construction
		public MemberForm(Member member, RecordAction action) : base(member, action, true) {}

		// Methods

		protected override void OnLoad(EventArgs ea)
		{
			base.OnLoad(ea);

			CheckSize();
			
			if (Action == RecordAction.Create)
				Text = "(new member)";
			else
				Text = String.Format("{0} {1}", ThisMember.FirstName, ThisMember.LastName);

			m_tabs = CreateTabsAndButtons("General", "History");
			TabControl.TabPageCollection pages = m_tabs.TabPages;

			int lastPos = 0;
			
			lastPos = AddLabelledControl(pages[0], lastPos + BORDER_SIZE, "Membership No.",	"Id");
			lastPos = AddLabelledControl(pages[0], lastPos + BORDER_SIZE, "Last Name",		"LastName"	);
			lastPos = AddLabelledControl(pages[0], lastPos + BORDER_SIZE, "First Name",		"FirstName"	);
			lastPos = AddLabelledControl(pages[0], lastPos + BORDER_SIZE, "Date of Birth",	"DateOfBirth",
				ValueBoxFlags.None, null, new ConvertEventHandler(DateOnlyFormatter), null);
			lastPos = AddLabelledControl(pages[0], lastPos + BORDER_SIZE, "Age",			    "Age",
				ValueBoxFlags.ReadOnly, null, null, null);
			lastPos = AddLabelledControl(pages[0], lastPos + BORDER_SIZE, "Home Address",	    "HomeAddress",
				ValueBoxFlags.Multiline, null, new ConvertEventHandler(MultilineFormatter), null);
			lastPos = AddLabelledControl(pages[0], lastPos + BORDER_SIZE, "Department",		 "Department");
            lastPos = AddLabelledControl(pages[0], lastPos + BORDER_SIZE, "Done Gym Induction?", "DoneGymInduction",
                ValueBoxFlags.IsBoolean, null, null, null); 
			
			m_detailsGrid = new DataGrid();
			m_detailsGrid.CaptionText	= "Details";
			m_detailsGrid.DataSource	= ThisMember.Details.GetAll();
			m_detailsGrid.Left			= BORDER_SIZE;
			m_detailsGrid.Top			= lastPos + BORDER_SIZE;
			m_detailsGrid.Width			= pages[0].Width - 2 * BORDER_SIZE;
			m_detailsGrid.Height		= pages[0].Height - (lastPos + 2 * BORDER_SIZE);

			SetStandardGridProperties(m_detailsGrid);
			pages[0].Controls.Add(m_detailsGrid);
			
			m_membershipsGrid = new DataGrid();
			m_membershipsGrid.CaptionText = "Memberships";
			m_membershipsGrid.DataSource = ThisMember.Memberships.GetAll();
			SetStandardGridProperties(m_membershipsGrid);
			pages[1].Controls.Add(m_membershipsGrid);
			
			m_paymentsGrid = new DataGrid();
			m_paymentsGrid.CaptionText = "Payments";
			m_paymentsGrid.DataSource = ThisMember.Payments.GetAll();
			SetStandardGridProperties(m_paymentsGrid);
			pages[1].Controls.Add(m_paymentsGrid);
			
			SetGridSizes();
		}

		protected override void OnResize(EventArgs ea)
		{
			CheckSize();
			SetGridSizes();
			base.OnResize(ea);
		}

		private void CheckSize()
		{
			if (Width < MIN_FORM_WIDTH || Height < MIN_FORM_HEIGHT)
			{
				Size = new Size(Math.Max(Width, MIN_FORM_WIDTH), Math.Max(Height, MIN_FORM_HEIGHT));
			}
		}
		
		protected override void SetStandardGridProperties(DataGrid grid)
		{
			base.SetStandardGridProperties(grid);
			
			grid.Enter			+= new EventHandler(Grid_Enter);
			grid.MouseEnter		+= new EventHandler(Grid_Enter);
		}
		
		private void Grid_Enter(object sender, EventArgs ea)
		{
			if (sender == m_detailsGrid)
			{
				CurrentDataSource = ThisMember.Details;
			}
			else if (sender == m_membershipsGrid)
			{
				CurrentDataSource = ThisMember.Memberships;
			}
			else if (sender == m_paymentsGrid)
			{
				CurrentDataSource = ThisMember.Payments;
			}
			else
			{
				CurrentDataSource = null;
			}
			
			if (CurrentDataSource == null)
			{
				CurrentData = null;
			}
			else
			{
				CurrentData = ((DataGrid)sender).DataSource;
			}
		}
		
		private void SetGridSizes()
		{
			if (m_tabs == null || m_tabs.TabCount < 2)
				return;
			
			int gridWidth	= m_tabs.TabPages[1].Width - 2 * BORDER_SIZE;
			int gridHeight	= (m_tabs.TabPages[1].Height - 3 * BORDER_SIZE) / 2;
			
			m_membershipsGrid.Left = BORDER_SIZE;
			m_membershipsGrid.Top =  BORDER_SIZE;
			m_membershipsGrid.Width = gridWidth;
			m_membershipsGrid.Height = gridHeight;
			
			m_paymentsGrid.Left = BORDER_SIZE;
			m_paymentsGrid.Top =  m_membershipsGrid.Bottom + BORDER_SIZE;
			m_paymentsGrid.Width = gridWidth;
			m_paymentsGrid.Height = gridHeight;
		}

		protected override object GetNewObject(IUpdatableDataSource ds)
		{
			if (ds is DetailCollection)
				return new Detail();
			
			if (ds is MembershipCollection)
				return new Membership();
			
			if (ds is PaymentCollection)
            {
				Payment p = new Payment();
                p.Amount = (Decimal)ThisMember.OwedCalc;
                return p;
            }
			
			throw new BaseException("Unexpected data source: " + ds.GetType().Name);
		}
		
		protected override Form GetActionForm(object o, RecordAction action)
		{
			Detail d = o as Detail;
			if (d != null)
				return new DetailForm(d, action);
			
			Membership m = o as Membership;
			if (m != null)
				return new MembershipForm(m, action);
			
			Payment p = o as Payment;
			if (p != null)
				return new PaymentForm(p, action);
			
			throw new BaseException("Unexpected data item: " + o.GetType().Name);
		}
		
		protected override void ActionCompleted(IUpdatableDataSource ds)
		{
			if (ds is DetailCollection)
				m_detailsGrid.DataSource = ThisMember.Details.GetAll();
			
			if (ds is MembershipCollection)
				m_membershipsGrid.DataSource = ThisMember.Memberships.GetAll();
			
			if (ds is PaymentCollection)
				m_paymentsGrid.DataSource = ThisMember.Payments.GetAll();
		}
	}
}