/*
 *  Project:        Membership Log (application using WhaTBI?)
 *  Filename:       FilterForm.cs
 *  Description:    Form to define filter conditions
 */

using System;
using System.Collections;
using System.Drawing;
using System.Windows.Forms;

using Pdbartlett.Whatbi;

namespace Pdbartlett.MembershipLog
{
    public class PropertiesComboBox : ComboBox
    {
        public PropertiesComboBox(ITypeInfo ti, string initName, bool showCollections)
        {
            if (ti == null)
                throw new ArgumentNullException();
            
            DropDownStyle = ComboBoxStyle.DropDownList;
            Sorted = true;
#if FX_V20            
            AutoSize = true;
#else
            Width = 150;
#endif
            IPropertyInfo init = null;
            
            foreach (IPropertyInfo pi in ti.GetProperties())
            {
                if (pi.TypeInfo.IsCollection)
                    continue;
                
                Items.Add(pi);
                if (pi.Name == initName)
                    init = pi;
            }
            
            if (Items.Count > 0)
            {
                if (init == null)
                    SelectedIndex = 0;
                else
                    SelectedItem = init;
            }
        }
    }
    
    public class FilterSpec
    {
        private IPredicate  m_predicate;
        private string      m_description;
        
        public FilterSpec(IPredicate predicate) : this(predicate, null) {}
        
        public FilterSpec(IPredicate predicate, string description)
        {
            m_predicate = predicate;
            m_description = description;
        }
        
        public IPredicate Predicate { get { return m_predicate; } }
        
        public string Description { get { return m_description; } }
        
        public override string ToString() { return Description; }
    }
    
    public abstract class BaseFilterForm : BaseForm
    {
        public abstract FilterSpec Filter { get; }
    }
    
    public class FilterForm : BaseFilterForm
    {
        // constants
        private const int MIN_FORM_WIDTH = 500;
        
        // data members
        private ITypeInfo m_ti;

        // controls
        private ComboBox m_cbProperties;
        private ComboBox m_cbOperator;
        private TextBox m_tbValue;

        public FilterForm(ITypeInfo ti, string text)
        {
            if (ti == null)
                throw new ArgumentNullException();

            m_ti = ti;
            Text = text;
        }

        public override FilterSpec Filter
        {
            get
            {
                try
                {
                    PredicateInfo comp = m_cbOperator.SelectedItem as PredicateInfo;
                    if (comp == null)
                        return null;
                    
                    IPropertyInfo prop = m_cbProperties.SelectedItem as IPropertyInfo;
                    if (prop == null)
                        return null;
                    
                    string value = m_tbValue.Text;
                    
                    IPredicate pred;
                    string desc = prop.DisplayName + " " + comp.Name;
                    
                    if (comp.Arity == 1)
                    {
                       pred = comp.BuildPropertyPredicate(prop);
                    }
                    else
                    {
                       pred = comp.BuildPropertyPredicate(prop, value);
                       desc += " " + value;
                    }
                    
                    return new FilterSpec(pred, desc);
                }
                catch (Exception /*e*/)
                {
                    MessageBox.Show("Unexpected problem applying filter; please check value type matches field type.",
                                    "Membership Log", MessageBoxButtons.OK, MessageBoxIcon.Error);
                                    
                    return null;
                }
            }
        }
        
        protected override void OnLoad(EventArgs ea)
        {
            base.OnLoad(ea);
            
            // form
            Width = MIN_FORM_WIDTH;
            
            // properties combo
            m_cbProperties = new PropertiesComboBox(m_ti, null, false);
            m_cbProperties.Top = BORDER_SIZE;
            m_cbProperties.Left = BORDER_SIZE;
            Controls.Add(m_cbProperties);

            // operators combo

            m_cbOperator = new ComboBox();
            m_cbOperator.DropDownStyle = ComboBoxStyle.DropDownList;
#if FX_V20            
            m_cbOperator.AutoSize = true;
#else
            m_cbOperator.Width = 200;
#endif
            m_cbOperator.Top = BORDER_SIZE;
            m_cbOperator.Left = m_cbProperties.Right + BORDER_SIZE;
            m_cbOperator.SelectedIndexChanged += new EventHandler(Operator_Changed);

            foreach (PredicateInfo comp in Expr.GetPredicateInfo())
                m_cbOperator.Items.Add(comp);

            if (m_cbOperator.Items.Count > 0)
                m_cbOperator.SelectedIndex = 0;
            
            Controls.Add(m_cbOperator);

            // value text
            
            m_tbValue = new TextBox();
            m_tbValue.Top = BORDER_SIZE;
            m_tbValue.Left = m_cbOperator.Right + BORDER_SIZE;
            m_tbValue.Width = ClientSize.Width - (m_tbValue.Left + BORDER_SIZE);
            m_tbValue.Anchor = AnchorStyles.Left | AnchorStyles.Right | AnchorStyles.Top;

            Controls.Add(m_tbValue);

            // buttons
            int buttonsBottom = CreateButtons(m_tbValue.Bottom);
            
            // explicit call handlers required to set up form correctly
            Operator_Changed(null, null);
        }
        
        private void Operator_Changed(object sender, EventArgs ea)
        {
            if (m_tbValue == null)
                return;
            
            PredicateInfo comp = m_cbOperator.SelectedItem as PredicateInfo;
            if (comp == null)
                return;
                
            m_tbValue.Enabled = comp.Arity > 1;  
        }
    }
}