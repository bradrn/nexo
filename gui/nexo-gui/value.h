#ifndef VALUE_H
#define VALUE_H

#include <QWidget>

class HsSheet;
class QLineEdit;

class Value : public QWidget
{
    Q_OBJECT
public:
    explicit Value(int key, HsSheet *sheet, QWidget *parent = nullptr);

public slots:
    void invalidate();
    void requery();

private:
    int key;
    HsSheet *sheet;

    QLayout *layout;

    QLineEdit *nameEdit;
    QLineEdit *typeEdit;
    QLineEdit *exprEdit;
    QWidget *valueDisplay;

    template<class W>
    W* getEditWidget();

    void setValueText(QString valueText);
};

#endif // VALUE_H
