/*
 *	Project:		Membership Log (application using WhaTBI?)
 *	Filename:		PaymentForm.cs
 *	Description:	Form to display, update and create payments
 */

using System;
using System.Drawing;
using System.Globalization;
using System.Windows.Forms;

using Pdbartlett.Whatbi;

namespace Pdbartlett.MembershipLog
{
	public class PaymentForm : BaseForm
	{
		// Construction
		public PaymentForm(Payment payment, RecordAction action) : base(payment, action, true) {}
		
		// Helper property
		private Payment ThisPayment { get { return (Payment)Subject; } }

		// Methods

		protected override void OnLoad(EventArgs ea)
		{
			base.OnLoad(ea);

			if (Action == RecordAction.Create)
				Text = "(new payment)";
			else
				Text = ThisPayment.Description;

			int lastPos = 0;
			
			lastPos = AddLabelledControl(this, lastPos + BORDER_SIZE, "Date", "PaymentDate",
				ValueBoxFlags.None, null, new ConvertEventHandler(DateOnlyFormatter), null);
			lastPos = AddLabelledControl(lastPos + BORDER_SIZE, "Amount", "Amount");
			lastPos = AddLabelledControl(lastPos + BORDER_SIZE, "Description", "Description");
			
			CreateButtons(lastPos);
		}
	}
}	
