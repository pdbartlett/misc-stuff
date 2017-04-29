/*
 *  Project:        Membership Log (application using WhaTBI?)
 *  Filename:       SettingsForm.cs
 *  Description:    Form to view/modify application settings
 */

using System;
using System.Collections;
using System.Drawing;
using System.Windows.Forms;

using Pdbartlett.Whatbi;

namespace Pdbartlett.MembershipLog
{
    public class DetailTypeForm : BaseForm
    {
        // Construction
        public DetailTypeForm(DetailType detailType, RecordAction action) : base(detailType, action, true) {}
        
        // Helper property
        private DetailType ThisDetailType { get { return (DetailType)Subject; } }

        // Methods

        protected override void OnLoad(EventArgs ea)
        {
            base.OnLoad(ea);

            if (Action == RecordAction.Create)
                Text = "(new detail type)";
            else
                Text = ThisDetailType.Name;

            int lastPos = 0;
            
            lastPos = AddLabelledControl(this, lastPos + BORDER_SIZE, "Name", "Name", ValueBoxFlags.InitOnly, null, null, null);
            
            CreateButtons(lastPos);
        }
    }
    
    public class MembershipTypeForm : BaseForm
    {
        // Construction
        public MembershipTypeForm(MembershipType memType, RecordAction action) : base(memType, action, true) {}
        
        // Helper property
        private MembershipType ThisMembershipType { get { return (MembershipType)Subject; } }

        // Methods

        protected override void OnLoad(EventArgs ea)
        {
            base.OnLoad(ea);

            if (Action == RecordAction.Create)
                Text = "(new membership type)";
            else
                Text = ThisMembershipType.Name;

            int lastPos = 0;
            
            lastPos = AddLabelledControl(this, lastPos + BORDER_SIZE, "Name", "Name", ValueBoxFlags.InitOnly, null, null, null);
            lastPos = AddLabelledControl(this, lastPos + BORDER_SIZE, "Cost", "Cost");
            
            CreateButtons(lastPos);
        }
    }
    
    public class SettingsForm : GridForm
    {
        // Members
        TabPage  m_generalPage;
        TabPage  m_detailsPage;
        TabPage  m_memTypesPage;
        DataGrid m_dgDetails;
        DataGrid m_dgMemberships;
        
        // Construction
        public SettingsForm() : base(Settings.Current, RecordAction.Update, true) {}
        
        // Helpers
        private Settings ThisSettings { get { return (Settings)Subject; } }

        // Methods
        
        protected override void OnLoad(EventArgs ea)
        {
            base.OnLoad(ea);
            
            // Form
            Text = "Options";
            
            // Tabs
            TabControl tabs = CreateTabsAndButtons("General", "Member Details", "Membership Types");
            m_generalPage  = tabs.TabPages[0];
            m_generalPage.VisibleChanged += new EventHandler(TabChanged);
            m_detailsPage  = tabs.TabPages[1];
            m_detailsPage.VisibleChanged += new EventHandler(TabChanged);
            m_memTypesPage = tabs.TabPages[2];
            m_memTypesPage.VisibleChanged += new EventHandler(TabChanged);
            
            // Placeholder general page
            Label lbl = new Label();
            lbl.Bounds = new Rectangle(new Point(0,0), m_generalPage.ClientSize);
            lbl.TextAlign = ContentAlignment.MiddleCenter;
            lbl.Text = "Currently no general settings,\nplease switch to another tab";
            m_generalPage.Controls.Add(lbl);

            // Detail types page
            m_dgDetails = new DataGrid();
            m_dgDetails.Dock = DockStyle.Fill;
            SetStandardGridProperties(m_dgDetails);
            m_detailsPage.Controls.Add(m_dgDetails);
            
            // Membership types page
            m_dgMemberships = new DataGrid();
            m_dgMemberships.Dock = DockStyle.Fill;
            SetStandardGridProperties(m_dgMemberships);
            m_memTypesPage.Controls.Add(m_dgMemberships);
            
            // Force population
            SetToDetails(true);
            SetToMembershipTypes(true);
        }
        
        private void TabChanged(object sender, EventArgs ea)
        {
            TabPage tab = (TabPage)sender;
            
            if (!tab.Visible)
                return;
            
            if (tab == m_detailsPage)
                SetToDetails(false);
            else if (tab == m_memTypesPage)
                SetToMembershipTypes(false);
        }
        
        private void SetToDetails(bool bRefresh)
        {
            if (bRefresh)
                m_dgDetails.DataSource = ThisSettings.Details.GetAll();
            
            CurrentData = m_dgDetails.DataSource;
            CurrentDataSource = ThisSettings.Details;
        }
        
        private void SetToMembershipTypes(bool bRefresh)
        {
            if (bRefresh)
                m_dgMemberships.DataSource = ThisSettings.Memberships.GetAll();
            
            CurrentData = m_dgMemberships.DataSource;
            CurrentDataSource = ThisSettings.Memberships;
        }
        
        protected override object GetNewObject(IUpdatableDataSource ds)
        {
            if (ds is DetailTypeCollection)
                return new DetailType();
            
            if (ds is MembershipTypeCollection)
                return new MembershipType();
            
            throw new BaseException("Unexpected data source type: " + ds.GetType().Name);
        }
        
        protected override Form GetActionForm(object obj, RecordAction action)
        {
            DetailType dt = obj as DetailType;
            if (dt != null)
                return new DetailTypeForm(dt, action);
            
            MembershipType mt = obj as MembershipType;
            if (mt != null)
                return new MembershipTypeForm(mt, action);
            
            throw new BaseException("Unexpected object type: " + obj.GetType().Name);
        }
        
        protected override void ActionCompleted(IUpdatableDataSource ds)
        {
            if (ds is DetailTypeCollection)
                SetToDetails(true);
            else if (ds is MembershipTypeCollection)
                SetToMembershipTypes(true);
            else
                throw new BaseException("Unexpected data source type: " + ds.GetType().Name);
        }
    }
}
