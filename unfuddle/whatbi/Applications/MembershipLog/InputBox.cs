/*
 *	Project:		Membership Log (application using WhaTBI?)
 *	Filename:		InputBox.cs
 *	Description:	Form to request single data item from user
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
	public delegate bool StringValidator(string value);
	
	public class InputBox : BaseForm
	{
		// members
		private string				m_prompt;
		private string				m_initial;
		private StringValidator		m_validator;
		private TextBox				m_textBox;
		
		public InputBox() : this("Enter value", "Value", "", null) {}
		public InputBox(string title, string prompt) : this (title, prompt, "", null) {}
		
		public InputBox(string title, string prompt, string initial, StringValidator validator)
		{
			Text = title;
			m_prompt = prompt;
			m_initial = initial;
			m_validator = validator;
		}

		protected override void OnLoad(EventArgs ea)
		{
			base.OnLoad(ea);
			
			int lastPos = AddLabelledControl(this, BORDER_SIZE, m_prompt, null, ValueBoxFlags.DontAlign, null, null, null);
			CreateButtons(lastPos);
			
			// install change handler on TextBox
			foreach (Control c in Controls)
			{
				TextBox tb = c as TextBox;
				if (tb == null)
					continue;
				
				m_textBox = tb;
				m_textBox.Text = m_initial;
				m_textBox.TextChanged += new EventHandler(Value_TextChanged);
				break;
			}
		}
		
		private void Value_TextChanged(object sender, EventArgs ea)
		{
			OkEnabled = (m_validator == null) || m_validator(Value);
		}
		
		protected override void OnSubmit(CancelEventArgs cea)
		{
			cea.Cancel = (m_validator != null) && !m_validator(Value);
			if (cea.Cancel)
			{
				MessageBox.Show("An invalid value was entered. Please retry.", "Error",
					MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
			}
		}

		public string Value
		{
			get { return m_textBox.Text; }
		}
	}
}