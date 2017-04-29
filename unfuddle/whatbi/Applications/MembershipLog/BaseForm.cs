/*
 *  Project:        Membership Log (application using WhaTBI?)
 *  Filename:       BaseForm.cs
 *  Description:    Common form code
 */

using System;
using System.Collections;
using System.ComponentModel;
using System.Reflection;
using System.Windows.Forms;

using Pdbartlett.Whatbi;

namespace Pdbartlett.MembershipLog
{
    public class BaseForm : Form
    {
        // Custom events

        public event CancelEventHandler Submit;

        // Enumerated data types

        public enum RecordAction { None, View, Update, Create, Delete };

        [Flags]
        protected enum ValueBoxFlags
        {
            None            = 0x00
        ,   ReadOnly        = 0x01
        ,   InitOnly        = 0x02
        ,   Multiline       = 0x04
        ,   DontAlign       = 0x08
        ,   BindComboText   = 0x10
        ,   IsBoolean       = 0x20
        };

        protected bool FlagSet(ValueBoxFlags flags, ValueBoxFlags reqd)
        {
            return ((flags & reqd) == reqd);
        }

        // String constants (should be moved out for globalisation)
        protected   const string    MENU_RECORD             = "&Record";
        protected   const string    MENU_RECORD_NEW         = "&New...";
        protected   const string    MENU_RECORD_VIEW        = "&View...";
        protected   const string    MENU_RECORD_UPDATE      = "&Update...";
        protected   const string    MENU_RECORD_DELETE      = "&Delete";

        // Numerical constants (some refactoring needed here!)
        protected   const int       BORDER_SIZE             = 3;
        private     const int       COLUMN_TWO              = 100;
        private     const int       MULTILINE_HEIGHT_FACTOR = 3;

        // Member variables
        private     object          m_object;
        private     RecordAction    m_action;

        // Controls (may or may not be created)
        private     Button          m_Ok;

        // Accessor properties
        protected   object          Subject     { get { return m_object; } }
        protected   RecordAction    Action      { get { return m_action; } }
        protected   bool            OkEnabled
        {
            get { return m_Ok != null && m_Ok.Enabled; }
            set { if (m_Ok == null) throw new InvalidOperationException(); m_Ok.Enabled = value; }
        }

        // Construction

        protected BaseForm() : this(null, RecordAction.None, false) {}

        protected BaseForm(object obj, RecordAction action, bool disallowNullNone)
        {
            if (!Enum.IsDefined(typeof(RecordAction), action))
                throw new ArgumentException();

            if (disallowNullNone && (obj == null || action == RecordAction.None))
                throw new ArgumentException();

            m_object = obj;
            m_action = action;
        }

        // Methods

        protected void OK_Click(object sender, EventArgs ea)
        {
            CancelEventArgs cea = new CancelEventArgs();
            OnSubmit(cea);
            if (cea.Cancel)
                return;

            DialogResult = DialogResult.OK;
            CompleteEdit();
            Hide();
        }

        protected void Cancel_Click(object sender, EventArgs ea)
        {
            DialogResult = DialogResult.Cancel;
            CancelEdit();
            Hide();
        }

        protected virtual void OnSubmit(CancelEventArgs cea)
        {
            if (Submit != null)
                Submit(this, cea);
        }

        private void CompleteEdit()
        {
            if (Subject != null)
                GetSubjectBindingManager().EndCurrentEdit();
        }

        private void CancelEdit()
        {
            if (Subject != null)
             GetSubjectBindingManager().CancelCurrentEdit();
        }

        private BindingManagerBase GetSubjectBindingManager()
        {
            if (Subject == null)
                throw new BaseException("No subject set");

            BindingManagerBase bm = BindingContext[Subject];
            if (bm == null)
                throw new BaseException("Subject not bound");

            return bm;
        }

        private void AddSubjectDataBinding(Control ctl, string propName, string dataName,
            ConvertEventHandler formatter, ConvertEventHandler parser)
        {
            Binding binding = new Binding(propName, Subject, dataName);

            if (formatter != null)
                binding.Format += formatter;
            if (parser != null)
                binding.Parse += parser;

            ctl.DataBindings.Add(binding);
        }

        protected int CreateButtons()
        {
            return CreateButtons(-1);
        }

        protected int CreateButtons(int fieldsBottom)
        {
            m_Ok = new Button();
            m_Ok.Text = (Action == RecordAction.View) ? "Close" : "OK";
            m_Ok.Left = BORDER_SIZE;

            int buttonTop = ClientSize.Height - m_Ok.Height - BORDER_SIZE;

            if (fieldsBottom == -1)
            {
                m_Ok.Top = buttonTop;
            }
            else
            {
                m_Ok.Top = fieldsBottom + BORDER_SIZE;
                int heightDelta = m_Ok.Top - buttonTop;
                Height += heightDelta;
            }

            m_Ok.Anchor = AnchorStyles.Left | AnchorStyles.Bottom;
            m_Ok.Click += new EventHandler(OK_Click);
            Controls.Add(m_Ok);
            AcceptButton = m_Ok;

            if (Action == RecordAction.View)
            {
                CancelButton = m_Ok;
            }
            else
            {
                Button cancel = new Button();
                cancel.Text = "Cancel";
                cancel.Left = m_Ok.Right + BORDER_SIZE;
                cancel.Top = m_Ok.Top;
                cancel.Anchor = AnchorStyles.Left | AnchorStyles.Bottom;
                cancel.Click += new EventHandler(Cancel_Click);
                Controls.Add(cancel);
                CancelButton = cancel;
            }

            return m_Ok.Top;
        }

        protected TabControl CreateTabsAndButtons(params string[] tabNames)
        {
            int buttonsTop = CreateButtons();

            TabControl tabs = new TabControl();
            tabs.Left = BORDER_SIZE;
            tabs.Top = BORDER_SIZE;
            tabs.Width = ClientSize.Width - 2 * BORDER_SIZE;
            tabs.Height = buttonsTop - tabs.Top - BORDER_SIZE;
            tabs.Anchor = AnchorStyles.Left | AnchorStyles.Right | AnchorStyles.Top | AnchorStyles.Bottom;

            TabPage[] pages = new TabPage[tabNames.Length];

            for (int i = 0; i < tabNames.Length; ++i)
                pages[i] = new TabPage(tabNames[i]);

            tabs.TabPages.AddRange(pages);

            Controls.Add(tabs);
            tabs.BringToFront();

            return tabs;
        }

        protected int AddLabelledControl(int top, string displayName, string dataName)
        {
            return AddLabelledControl(this, top, displayName, dataName);
        }

        protected int AddLabelledControl(Control container, int top, string displayName, string dataName)
        {
            return AddLabelledControl(container, top, displayName, dataName, ValueBoxFlags.None, null, null, null);
        }

        protected int AddLabelledControl(Control container, int top, string displayName, string dataName,
            ValueBoxFlags flags, ICollection values, ConvertEventHandler formatter, ConvertEventHandler parser)
        {
            // Label
            
            Label lbl = null;
            bool isBoolean = FlagSet(flags, ValueBoxFlags.IsBoolean);
            
            if (!isBoolean)
            {
                lbl = new Label();
                lbl.Text = String.Format("{0}:", displayName);
                lbl.AutoSize = true;
                lbl.Left = BORDER_SIZE;
                lbl.Top = top;          // for now
                lbl.Visible = false;    // likewise
    
                container.Controls.Add(lbl);
            }
            
            // Control

            Control     ctl = null;
            ComboBox    cb = null;
            string      propName;
            
            bool isReadOnly =
                m_action == RecordAction.View ||
                FlagSet(flags, ValueBoxFlags.ReadOnly) ||
                (m_action != RecordAction.Create && FlagSet(flags, ValueBoxFlags.InitOnly));

            if (isBoolean)
            {
                CheckBox chk = new CheckBox();
                chk.Text = displayName;
                chk.Enabled = !isReadOnly;

                ctl = chk;
                propName = "Checked";
            }
            else
            {
                if (isReadOnly || values == null || values.Count == 0)
                {
                    TextBox tb = new TextBox();
                    tb.ReadOnly = isReadOnly;
    
                    if (FlagSet(flags, ValueBoxFlags.Multiline))
                    {
                        tb.Multiline        =   true;
                        tb.AcceptsReturn    =   true;
                        tb.ScrollBars       =   ScrollBars.Both;
                        tb.Height           *=  MULTILINE_HEIGHT_FACTOR;
                    }
    
                    ctl = tb;
                    propName = "Text";
                }
                else
                {
                    cb = new ComboBox();
                    cb.DropDownStyle = ComboBoxStyle.DropDownList;
    
                    foreach (object o in values)
                        cb.Items.Add(o);
    
                    ctl = cb;
                    propName = "SelectedItem";
                }
            }

            ctl.Top     = top;
            ctl.Left    = FlagSet(flags, ValueBoxFlags.DontAlign) ? (lbl.Right + BORDER_SIZE) : COLUMN_TWO;
            ctl.Width   = container.ClientSize.Width - (ctl.Left + BORDER_SIZE);
            ctl.Anchor  = AnchorStyles.Left | AnchorStyles.Top | AnchorStyles.Right;
            ctl.Tag     = dataName;

            container.Controls.Add(ctl);

            if (dataName != null)
                AddSubjectDataBinding(ctl, propName, dataName, formatter, parser);

            if (cb != null && FlagSet(flags, ValueBoxFlags.BindComboText))
            {
                IExpression valExpr = Expr.GetProperty(dataName);
                object value = valExpr.Evaluate(Subject);

                if (value != null)
                {
                    string textValue = value.ToString();

                    foreach (object o in cb.Items)
                    {
                        if (o.ToString() == textValue)
                        {
                            cb.SelectedItem = o;
                            break;
                        }
                    }

                    if (cb.SelectedIndex == -1 && cb.Items.Count > 0)
                        cb.SelectedIndex = 0;
                }
            }

            if (lbl != null)
            {
                // can now calculate "correct" top position for label, and make visible
                lbl.Top = (ctl.Top + ctl.Bottom - lbl.Height) / 2;
                lbl.Visible = true;
            }

            return ctl.Bottom;
        }

        protected void DateOnlyFormatter(object sender, ConvertEventArgs cea)
        {
            if (cea.DesiredType     == typeof(string) &&
                cea.Value.GetType() == typeof(DateTime))
            {
                DateTime dt = (DateTime)cea.Value;
                cea.Value = dt.ToShortDateString();
            }
        }

        protected void MultilineFormatter(object sender, ConvertEventArgs cea)
        {
            if (cea.DesiredType     == typeof(string) &&
                cea.Value.GetType() == typeof(string))
            {
                string s = (string)cea.Value;
                s = s.Replace("\r\n", "\n");            // normalize
                cea.Value = s.Replace("\n", "\r\n");    // format
            }
        }
    }
}