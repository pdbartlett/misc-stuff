/*
 *  Project:        Membership Log (application using WhaTBI?)
 *  Filename:       FilterForm.cs
 *  Description:    Form to define filter conditions
 */

using System;
using System.Collections;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;

using Pdbartlett.Whatbi;

namespace Pdbartlett.MembershipLog
{
    public class SortForm : BaseForm
    {
        // data members
        private ITypeInfo       m_ti;
        private string          m_propName;
        private IPropertyInfo   m_pi;
        private bool            m_ascending;

        // controls
        PropertiesComboBox  m_pcb;
        CheckBox            m_chkAsc;

        public SortForm(ITypeInfo ti, SortSpec spec)
        {
            if (ti == null)
                throw new ArgumentNullException();

            m_ti = ti;
            
            if (spec != null)
            {
                m_propName = spec.PropName;
                m_ascending = spec.Ascending;
            }
            else
                m_ascending = true;
        }
        
        public SortSpec SortSpec { get { return new SortSpec(m_pi.Name, m_ascending); } }
        
        protected override void OnLoad(EventArgs ea)
        {
            base.OnLoad(ea);
            
            // form
            Text = "Sort";
            
            // field
            
            Label lblField = new Label();
            lblField.Visible = true;
            lblField.Top = BORDER_SIZE;
            lblField.Left = BORDER_SIZE;
            lblField.Text = "Sort by:";
            lblField.AutoSize = true;
            Controls.Add(lblField);
            
            m_pcb = new PropertiesComboBox(m_ti, m_propName, false);
            m_pcb.Top = BORDER_SIZE;
            m_pcb.Left = lblField.Right + BORDER_SIZE;
            m_pcb.Width = ClientSize.Width - (m_pcb.Left + BORDER_SIZE);
            m_pcb.Anchor = AnchorStyles.Left | AnchorStyles.Right | AnchorStyles.Top;
            Controls.Add(m_pcb);
            
            lblField.Top = m_pcb.Top + (m_pcb.Height - lblField.Height) / 2;
            lblField.Visible = true;
            
            // ascending
            m_chkAsc = new CheckBox();
            m_chkAsc.Top = m_pcb.Bottom + BORDER_SIZE;
            m_chkAsc.Left = BORDER_SIZE;
            m_chkAsc.Width = ClientSize.Width - 2 * BORDER_SIZE;
            m_chkAsc.Anchor = AnchorStyles.Left | AnchorStyles.Right | AnchorStyles.Top;
            m_chkAsc.Text = "Ascending?";
            m_chkAsc.TextAlign = ContentAlignment.MiddleLeft;
            m_chkAsc.Checked = m_ascending;
            Controls.Add(m_chkAsc);
            
            // buttons
            int buttonsTop = CreateButtons(m_chkAsc.Bottom);
        }
        
        protected override void OnSubmit(CancelEventArgs cea)
        {
            m_pi = (IPropertyInfo)m_pcb.SelectedItem;
            m_ascending = m_chkAsc.Checked;
            
            base.OnSubmit(cea);
        }
    }
}