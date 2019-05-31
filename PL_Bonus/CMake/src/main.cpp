#include <cstdlib>
#include <nana/gui.hpp>
#include <nana/gui/widgets/label.hpp>
#include <nana/gui/widgets/button.hpp>
#include <nana/gui/widgets/textbox.hpp>
#include <fmt/format.h>

int main()
{
    using namespace nana;

    //Define a form.
    form fm;

    textbox text = nana::textbox(fm);

    //Define a button and answer the click event.
    button btn{fm, u8"算!"};
    btn.events().click([&fm, &text]{
        auto val = atoi(text.caption().c_str());
        
        auto msg = msgbox(fm, u8"計算結果");
        (msg<<fmt::format("{0}*{0}={1}", val, val*val)).show();

    });

    fm.events().unload([&fm] {
        
        auto msg = msgbox(fm, u8"重要問題?", msgbox::yes_no);
        msg<<u8"胡老師美嗎?";
        do {
            if(msg.show()==msgbox::pick_yes)
                break;
            auto msg2 = msgbox(fm, u8"訊息");
            msg2.icon(msgbox::icon_information);
            (msg2<<u8"當學生要誠實").show();
        } while(true);
    
    });

    //Layout management
    fm.div("vert <><<><weight=80% text><>><><weight=24<><button><>><>");
    fm["text"]<<text;
    fm["button"] << btn;
    fm.collocate();
	
    //Show the form
    fm.show();

    //Start to event loop process, it blocks until the form is closed.
    exec();
}