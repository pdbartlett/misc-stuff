// maindlg.cpp : implementation of the CMainDlg class
//
/////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "resource.h"

#include "maindlg.h"

LRESULT CMainDlg::OnInitDialog(HWND /*hWnd*/, LPARAM /*lp*/)
{
	// center the dialog on the screen
	CenterWindow();

	// attach control wrappers
	m_edName.Attach(GetDlgItem(IDC_NAME));

	return TRUE;
}

LRESULT CMainDlg::OnOK(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	CString strMsg;
	int cch = 0;

	if (m_edName.IsWindow())
		cch = m_edName.GetWindowTextLength();
	
	if (cch > 0)
	{
		CString strName;
		m_edName.GetWindowText(strName.GetBuffer(cch+1), cch+1);

		strMsg.Format(_T("Hello, %s!"), strName);
	}
	else
		strMsg = _T("Don't be so shy!");

	::MessageBox(m_hWnd, strMsg, _T("Greetings!"), MB_OK);

	return 0;
}

LRESULT CMainDlg::OnCancel(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	EndDialog(wID);
	return 0;
}
