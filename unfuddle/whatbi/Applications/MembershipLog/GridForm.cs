/*
 *  Project:        Membership Log (application using WhaTBI?)
 *  Filename:       BaseForm.cs
 *  Description:    Common form code
 */

using System;
using System.Collections;
using System.Drawing;
using System.IO;
using System.Runtime.Serialization.Formatters.Soap;
using System.Windows.Forms;

using Pdbartlett.Whatbi;

namespace Pdbartlett.MembershipLog
{
    public abstract class GridForm : BaseForm
    {
        // Common controls
        private     ContextMenu     m_recordPopup;
        private     ContextMenu     m_columnPopup;

        // Member variables
        private     int             m_hitRow;
        private     int             m_hitCol;
        private     object          m_currentData;
        private     IDataSource     m_currentDataSource;

        // Accessor properties
        protected   int             HitRow              { get { return m_hitRow;            }                                           }
        protected   int             HitCol              { get { return m_hitCol;            }                                           }
        protected   ContextMenu     RecordPopup         { get { return m_recordPopup;       }                                           }
        protected   ContextMenu     ColumnPopup         { get { return m_columnPopup;       }   set { m_columnPopup = value;        }   }
        protected   object          CurrentData         { get { return m_currentData;       }   set { m_currentData = value;        }   }
        protected   IDataSource     CurrentDataSource   { get { return m_currentDataSource; }   set { m_currentDataSource = value;  }   }
        
        protected IUpdatableDataSource Updatable
        {
            get
            {
                IUpdatableDataSource updatable = m_currentDataSource as IUpdatableDataSource;
                if (updatable == null)
                    throw new BaseException("Cannot update this data source");
                
                return updatable;
            }
        }

        // Construction

        public GridForm() : base(null, RecordAction.None, false) {}
        public GridForm(object obj, RecordAction action, bool disallowNullNone) : base(obj, action, disallowNullNone) {}

        // Methods
        
        protected abstract object   GetNewObject(IUpdatableDataSource ds);
        protected abstract Form     GetActionForm(object o, RecordAction action);
        protected abstract void     ActionCompleted(IUpdatableDataSource ds);

        protected override void OnLoad(EventArgs ea)
        {
            base.OnLoad(ea);

            m_recordPopup = new ContextMenu();
            m_recordPopup.Popup += new EventHandler(RecordPopup_Popup);
            m_recordPopup.MenuItems.Add(new MenuItem(MENU_RECORD_NEW, new EventHandler(Action_New)));
            m_recordPopup.MenuItems.Add(new MenuItem(MENU_RECORD_VIEW, new EventHandler(Action_View)));
            m_recordPopup.MenuItems.Add(new MenuItem(MENU_RECORD_UPDATE, new EventHandler(Action_Update)));
            m_recordPopup.MenuItems.Add(new MenuItem(MENU_RECORD_DELETE, new EventHandler(Action_Delete)));
        }
        
        protected virtual void SetStandardGridProperties(DataGrid grid)
        {
            grid.ReadOnly       = true;
            grid.ContextMenu    = RecordPopup;
            grid.DoubleClick    += new EventHandler(Grid_DoubleClick);
            grid.MouseDown      += new MouseEventHandler(Grid_MouseDown);
        }
        
        private void RecordPopup_Popup(object sender, EventArgs ea)
        {
            UpdateRecordMenu(m_recordPopup);
        }

        protected void Action_View(object sender, EventArgs ea)
        {
            DoAction(RecordAction.View);
        }

        protected void Action_Update(object sender, EventArgs ea)
        {
            DoAction(RecordAction.Update);
        }

        protected void Action_New(object sender, EventArgs ea)
        {
            DoAction(RecordAction.Create);
        }

        protected void Action_Delete(object sender, EventArgs ea)
        {
            DoAction(RecordAction.Delete);
        }
        
        protected void Grid_MouseDown(object sender, MouseEventArgs mea)
        {
            DataGrid grid = sender as DataGrid;
            if (grid == null)
                return;
            
            DataGrid.HitTestInfo hti = grid.HitTest(mea.X, mea.Y);
            m_hitRow = hti.Row;
            m_hitCol = hti.Column;

            switch(hti.Type)
            {
                case DataGrid.HitTestType.Cell:
                case DataGrid.HitTestType.RowHeader:
                    grid.CurrentRowIndex = hti.Row;
                    break;
                case DataGrid.HitTestType.ColumnHeader:
                    if (mea.Button == MouseButtons.Right && ColumnPopup != null)
                        ColumnPopup.Show(this, new Point(mea.X, mea.Y));
                    break;
            }
        }
        
        protected void Grid_DoubleClick(object sender, EventArgs ea)
        {
            if (HitRow >= 0)
                Action_View(null, null);
        }

        protected virtual void UpdateRecordMenu(Menu m)
        {
            bool isItemSelected = (GetCurrent() != null);
            foreach (MenuItem mi in m.MenuItems)
            {
                if (Action == RecordAction.View || !(CurrentDataSource is IUpdatableDataSource))
                {
                    if (mi.Text == MENU_RECORD_VIEW)
                        mi.Enabled = isItemSelected;
                    else
                        mi.Enabled = false;
                }
                else
                {
                    if (mi.Text == MENU_RECORD_NEW)
                        mi.Enabled = true;
                    else
                        mi.Enabled = isItemSelected;
                }
            }
        }
        
        protected virtual void DoAction(RecordAction action)
        {
            try
            {
                object o;
                
                if (action == RecordAction.Create)
                    o = GetNewObject(Updatable);
                else
                    o = GetCurrent();
                
                if (o == null)
                    return;

                object initKey = Utility.GetKey(o);
                
                if (action == RecordAction.Delete)
                {
                    if (MessageBox.Show("Delete the current record?", "Confirm", MessageBoxButtons.YesNo,
                                        MessageBoxIcon.Question, MessageBoxDefaultButton.Button2) != DialogResult.Yes)
                        return;
                        
                    Updatable.Delete(initKey);
                }
                else
                {
                    using (Form f = GetActionForm(o, action))
                    {
                        if (f.ShowDialog() != DialogResult.OK || action == RecordAction.View)
                            return;
                        
                        switch(action)
                        {
                            case RecordAction.Create:
                                Updatable.Insert(o);
                                break;
                            
                            case RecordAction.Update:
                            {
                                object newKey = Utility.GetKey(o);
                                if (newKey != initKey)
                                {
                                    Updatable.Delete(initKey);
                                    Updatable.Insert(o);
                                }
                                else
                                {
                                    Updatable.Update(o);
                                }
                                break;
                            }
                                
                            default:
                                // shouldn't be possible, but just in case...
                                return;
                        }
                    }
                }
                
                ActionCompleted(Updatable);
            }
            catch (Exception e)
            {
                MessageBox.Show("Problem carrying out requested action:\n\n" + e.Message, "Error",
                    MessageBoxButtons.OK, MessageBoxIcon.Error); 
            }
        }

        protected CurrencyManager GetCurrentCurrencyManager()
        {
            if (CurrentData == null)
                throw new BaseException("No current datasource");
            
            CurrencyManager cm = BindingContext[CurrentData] as CurrencyManager;
            if (cm == null)
                throw new BaseException("Current datasource is not bound");
            
            return cm;
        }

        protected object GetCurrent()
        {
            CurrencyManager cm = GetCurrentCurrencyManager();
            
            if (cm.Position < 0)
                return null;

            return cm.Current;
        }
    }
}