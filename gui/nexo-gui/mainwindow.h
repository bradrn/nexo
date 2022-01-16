#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <QVector>

class HsSheet;
class QMdiArea;
class Value;

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    MainWindow(QWidget *parent = nullptr);
    ~MainWindow();

protected:
    void contextMenuEvent (QContextMenuEvent *event) override;

private slots:
    void newValue();
    void newInputList();
    void newTable();

private:
    void openSheet();
    void saveSheet();

    QMdiArea *mdiArea;

    int latestKey;

    QAction *newValueAct;
    QAction *newInputListAct;
    QAction *newTableAct;

    HsSheet *sheet;
};
#endif // MAINWINDOW_H
