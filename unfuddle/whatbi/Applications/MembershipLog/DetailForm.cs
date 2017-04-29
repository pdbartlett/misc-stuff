/*
 *	Project:		Membership Log (application using WhaTBI?)
 *	Filename:		DetailForm.cs
 *	Description:	Form to display, update and create custom details
 */

using System;
using System.Drawing;
using System.Globalization;
using System.Windows.Forms;

using Pdbartlett.Whatbi;

namespace Pdbartlett.MembershipLog
{
	public class DetailForm : BaseForm
	{
		// Construction
		public DetailForm(Detail detail, RecordAction action) : base(detail, action, true) {}
		
		// Helper property
		private Detail ThisDetail { get { return (Detail)Subject; } }

		// Methods

		protected override void OnLoad(EventArgs ea)
		{
			base.OnLoad(ea);

			if (Action == RecordAction.Create)
				Text = "(new detail)";
			else
				Text = ThisDetail.Name;

			int lastPos = 0;
			
			lastPos = AddLabelledControl(this, lastPos + BORDER_SIZE, "Name", "Name", ValueBoxFlags.InitOnly,
				Settings.Current.Details, null, null);
			lastPos = AddLabelledControl(lastPos + BORDER_SIZE, "Value", "Value");
			
			CreateButtons(lastPos);
		}
	}
}	
