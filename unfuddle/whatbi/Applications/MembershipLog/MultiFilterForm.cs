/*
 *	Project:		Membership Log (application using WhaTBI?)
 *	Filename:		FilterForm.cs
 *	Description:	Form to define filter conditions
 */

using System;
using System.Collections;
using System.Drawing;
using System.Windows.Forms;

using Pdbartlett.Whatbi;

namespace Pdbartlett.MembershipLog
{
	public class MultiFilterForm : BaseFilterForm
	{
		// data members
		private ITypeInfo	m_ti;

		// controls
		private ContextMenu	m_ctxMenu;
		private CheckBox	m_chkAll;
		private ListBox		m_lbFilters;

		public MultiFilterForm(ITypeInfo ti)
		{
			if (ti == null)
				throw new ArgumentNullException();

			m_ti = ti;
		}

		public override FilterSpec Filter
		{
			get
			{
				IPredicate pred = null;
				
				foreach (object o in m_lbFilters.Items)
				{
					FilterSpec fs = (FilterSpec)o;
					
					if (pred == null)
						pred = fs.Predicate;
					else if (m_chkAll.Checked)
						pred = Expr.And(pred, fs.Predicate);
					else
						pred = Expr.Or(pred, fs.Predicate);
				}
				
				return new FilterSpec(pred);
			}
		}
		
		protected override void OnLoad(EventArgs ea)
		{
			base.OnLoad(ea);
			
			// form
			Text = "Advanced filter";
			
			// buttons
			int buttonsTop = CreateButtons();

			// match all?
			m_chkAll = new CheckBox();
			m_chkAll.Checked = true;
			m_chkAll.Text = "Match all the above?";
			m_chkAll.TextAlign = ContentAlignment.MiddleLeft;
			m_chkAll.Top = buttonsTop - (m_chkAll.Height + BORDER_SIZE);
			m_chkAll.Left = BORDER_SIZE;
			m_chkAll.Width = ClientSize.Width - 2 * BORDER_SIZE;
			m_chkAll.Anchor = AnchorStyles.Left | AnchorStyles.Right | AnchorStyles.Bottom;
			Controls.Add(m_chkAll);
			
			// context menu
			m_ctxMenu = new ContextMenu();
			m_ctxMenu.Popup += new EventHandler(ContextMenu_Popup);
			m_ctxMenu.MenuItems.Add(new MenuItem(MENU_RECORD_NEW, new EventHandler(Filters_New)));
			m_ctxMenu.MenuItems.Add(new MenuItem(MENU_RECORD_DELETE, new EventHandler(Filters_Delete)));
			
			// filters
			m_lbFilters = new ListBox();
			m_lbFilters.Top = BORDER_SIZE;
			m_lbFilters.Left = BORDER_SIZE;
			m_lbFilters.Width = ClientSize.Width - 2 * BORDER_SIZE;
			m_lbFilters.Height = m_chkAll.Top - 2 * BORDER_SIZE;
			m_lbFilters.Anchor = AnchorStyles.Left | AnchorStyles.Right | AnchorStyles.Top | AnchorStyles.Bottom;
			m_lbFilters.ContextMenu = m_ctxMenu;
			Controls.Add(m_lbFilters);
		}
		
		private void Filters_New(object sender, EventArgs ea)
		{
			using (FilterForm form = new FilterForm(m_ti, "Add filter condition"))
			{
				if (form.ShowDialog() != DialogResult.OK)
					return;
				
				m_lbFilters.Items.Add(form.Filter);
			}
		}
		
		private void Filters_Delete(object sender, EventArgs ea)
		{
			object sel = m_lbFilters.SelectedItem;
			if (sel != null)
				m_lbFilters.Items.Remove(sel);
		}
		
		private void ContextMenu_Popup(object sender, EventArgs ea)
		{
			foreach (MenuItem mi in m_ctxMenu.MenuItems)
			{
				switch (mi.Text)
				{
					case MENU_RECORD_NEW:
						mi.Enabled = true;
						break;
					case MENU_RECORD_DELETE:
						mi.Enabled = (m_lbFilters.SelectedItem != null);
						break;
				}
			}
		}
	}
}