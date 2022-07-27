#include "Neat_Spikes_App.h"
#include "MainFrame.h"
#include <wx/wx.h>

wxIMPLEMENT_APP(Neat_Spikes_App);

bool Neat_Spikes_App::OnInit() {
	MainFrame* mainFrame = new MainFrame("HIII");
	mainFrame->Show();
	mainFrame->Center();
	return true;
}
