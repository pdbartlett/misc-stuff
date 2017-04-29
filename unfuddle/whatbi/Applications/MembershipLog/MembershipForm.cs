/*
 *  Project:        Membership Log (application using WhaTBI?)
 *  Filename:       MembershipForm.cs
 *  Description:    Form to display, update and create memberships
 */

using System;
using System.Drawing;
using System.Globalization;
using System.Windows.Forms;

using Pdbartlett.Whatbi;

namespace Pdbartlett.MembershipLog
{
    public class MembershipForm : BaseForm
    {
        // Construction
        public MembershipForm(Membership membership, RecordAction action) : base(membership, action, true) {}
        
        // Helper property
        private Membership ThisMembership { get { return (Membership)Subject; } }

        // Methods

        protected override void OnLoad(EventArgs ea)
        {
            base.OnLoad(ea);

            if (Action == RecordAction.Create)
                Text = "(new membership)";
            else
                Text = ThisMembership.MembershipType;

            int lastPos = 0;
            
            lastPos = AddLabelledControl(this, lastPos + BORDER_SIZE, "Type", "MembershipType",
                ValueBoxFlags.BindComboText, Settings.Current.Memberships, null, null);
            lastPos = AddLabelledControl(this, lastPos + BORDER_SIZE, "Start date", "StartDate",
                ValueBoxFlags.None, null, new ConvertEventHandler(DateOnlyFormatter), null);
            lastPos = AddLabelledControl(this, lastPos + BORDER_SIZE, "End date", "EndDate",
                ValueBoxFlags.None, null, new ConvertEventHandler(DateOnlyFormatter), null);
            lastPos = AddLabelledControl(lastPos + BORDER_SIZE, "Cost", "Cost");
            
            foreach (Control c in Controls)
            {
                ComboBox cb = c as ComboBox;
                if (cb == null)
                    continue;
                
                cb.SelectedIndexChanged += new EventHandler(Type_Changed);
                
                if (Action == RecordAction.Create)
                    Type_Changed(cb, EventArgs.Empty); // force initial cost alignment
                    
                break;
            }
            
            CreateButtons(lastPos);
        }
        
        private void Type_Changed(object sender, EventArgs ea)
        {
            ComboBox cb = (ComboBox)sender;
            MembershipType type = cb.SelectedItem as MembershipType;
            if (type != null)
                ThisMembership.Cost = type.Cost;
        }
    }
}