/*
 *  Project:        Membership Log (application using WhaTBI?)
 *  Filename:       ListSelectionForm.cs
 *  Description:    Form to request a user to select one or more items
 */

using System;
using System.Collections;
using System.ComponentModel;
using System.Drawing;
using System.Globalization;
using System.Windows.Forms;

using Pdbartlett.Whatbi;

namespace Pdbartlett.MembershipLog
{
    public class ListSelectionForm : BaseForm
    {
        // members
        private ICollection m_data;
        
        // controls
        private ListBox     m_list;
        
        public ListSelectionForm(ICollection data) : this(data, "Select value") {}
        
        public ListSelectionForm(ICollection data, string title)
        {
            Text = title;
            m_data = data;
        }
        
        public object SelectedItem
        {
            get
            {
                return m_list.SelectedItem;
            }
        }

        protected override void OnLoad(EventArgs ea)
        {
            base.OnLoad(ea);
            
            int buttonTop = CreateButtons();
            
            m_list = new ListBox();
            m_list.Top = BORDER_SIZE;
            m_list.Left = BORDER_SIZE;
            m_list.Width = ClientSize.Width - 2 * BORDER_SIZE;
            m_list.Height = buttonTop - 2 * BORDER_SIZE;
            m_list.Anchor = AnchorStyles.Left | AnchorStyles.Right | AnchorStyles.Top | AnchorStyles.Bottom;
            m_list.DoubleClick += new EventHandler(OK_Click); // wire up directly to OK button click handler!
            
            foreach (object o in m_data)
                m_list.Items.Add(o);
                
            if (m_list.Items.Count > 0)
                m_list.SelectedIndex = 0;
            
            Controls.Add(m_list);
        }
    }
}