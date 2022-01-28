#ifndef VALUE_H
#define VALUE_H

#include <QWidget>

class HsCell;
class HsSheet;
class QLineEdit;

class Value : public QWidget
{
    Q_OBJECT
public:
    explicit Value(int key, HsSheet *sheet, QWidget *parent = nullptr);

    void loadValueFrom(const HsCell &cell);

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
    void setErrorText(QString errorText);
};

#endif // VALUE_H
