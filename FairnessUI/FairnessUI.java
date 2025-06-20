import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.concurrent.*;
import java.util.concurrent.atomic.*;
import javax.swing.*;

public class FairnessUI {
    public static void main(String[] args) {
        try {
            final String os = System.getProperty("os.name");
            if (os != null && (os.toLowerCase().contains("win") || os.toLowerCase().contains("mac"))) {
                UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
            } else {
                UIManager.setLookAndFeel(UIManager.getCrossPlatformLookAndFeelClassName());
            }
        } catch (Exception e) {
            System.err.println("Error during setup:\n");
            e.printStackTrace();

            return;
        }

        // In order for this to run, "COMBINEDCARLIST.txt" must be on the current working directory, and
        // "SpecII-Fairness-Tester" must be findable via the system PATH or current working directory.

        try {
            final var cars = Files
                .readAllLines(Paths.get("COMBINEDCARLIST.txt"))
                .stream()
                .map(line -> line.split("\\|")[1])
                .toList();

            SwingUtilities.invokeLater(() -> {
                final CounterFrame frame = new CounterFrame(cars);
                frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
                frame.pack();
                frame.setLocationRelativeTo(null);
                frame.setVisible(true);
            });
        } catch (IOException e) {
            System.err.println("Failed to run IO operation:\n");
            e.printStackTrace();

            return;
        }
    }

    // -1 = 255.
    // -128 = 128.
    private static int byteExtend(byte b) {
        if (b >= 0) return ((int) b);
        else return (256 + (int) b);
    }

    private record CarResult(JLabel name, JLabel amount, JLabel percentageText) {}

    private static abstract class ScrollableListenerPane extends JScrollPane implements ActionListener {
        public ScrollableListenerPane(Component c) {
            super(c);
        }
    }

    private static class CounterFrame extends JFrame {
        private final ExecutorService ioWorker = Executors.newSingleThreadExecutor();
        private final AtomicLong[] carCounters;
        private final AtomicLong carsObtained;

        public CounterFrame(java.util.List<String> cars) {
            super("Randomizer Car Counter");
            setLayout(new BorderLayout());

            carCounters = new AtomicLong[cars.size()];
            for (int i = 0; i < cars.size(); ++i) {
                carCounters[i] = new AtomicLong(0);
            }

            carsObtained = new AtomicLong(0);

            addWindowListener(new WindowAdapter() {
                @Override
                public void windowClosing(WindowEvent e) {
                    super.windowClosing(e);

                    if (e.getNewState() == WindowEvent.WINDOW_CLOSING) {
                        ioWorker.shutdownNow();
                    }
                }
            });

            final java.util.List<CarResult> entries = cars
                .stream()
                .map(cn -> new CarResult(new JLabel(cn), new JLabel("0x"), new JLabel("(0.00%)")))
                .toList();

            final var holdingPanel = new JPanel();
            holdingPanel.setLayout(new FlowLayout(FlowLayout.LEFT, 8 ,0));

            final var namesPanel = new JPanel();
            namesPanel.setLayout(new GridLayout(cars.size(), 1, 0, 2));

            final var amountPanel = new JPanel();
            amountPanel.setLayout(new GridLayout(cars.size(), 1, 0, 2));

            final var percentagePanel = new JPanel();
            percentagePanel.setLayout(new GridLayout(cars.size(), 1, 0, 2));

            entries.forEach(car -> {
                namesPanel.add(car.name);
                amountPanel.add(car.amount);
                percentagePanel.add(car.percentageText);
            });

            holdingPanel.add(namesPanel);
            holdingPanel.add(amountPanel);
            holdingPanel.add(percentagePanel);

            final var resultsPane = new ScrollableListenerPane(holdingPanel) {
                @Override
                public void actionPerformed(ActionEvent e) {
                    if (e.getSource().getClass().equals(Timer.class)) {
                        for (int i = 0; i < cars.size(); ++i) {
                            final long carsGotten = carsObtained.get();
                            final long thisCarGotten = carCounters[i].get();

                            final double proportion = (double) thisCarGotten / (double) carsGotten;

                            entries.get(i).amount.setText(thisCarGotten + "x");
                            if (carsGotten > 0) {
                                entries.get(i).percentageText.setText("(" + String.format("%.2f", proportion * 100.0) + "%)");
                            }
                        }
                    }
                }
            };

            final Timer updateTimer = new Timer(1000, resultsPane);
            updateTimer.setRepeats(true);

            resultsPane.getVerticalScrollBar().setUnitIncrement(16);

            add(resultsPane, BorderLayout.CENTER);

            ioWorker.submit(() -> {
                try {
                    final Process fairnessTester = new ProcessBuilder(java.util.List.of("SpecII-Fairness-Tester", "--simple")).start();
                    final byte[] buffer = new byte[2];

                    try (var stdout = new BufferedInputStream(fairnessTester.getInputStream())) {
                        do {
                            var charsRead = stdout.readNBytes(buffer, 0, 2);

                            if (charsRead < 2) throw new IOException("Not enough bytes available.");
                            if (buffer[0] == -128 && buffer[1] == -128) break;

                            int carNumber = (byteExtend(buffer[0]) << 8) + byteExtend(buffer[1]);
                            carsObtained.incrementAndGet();
                            carCounters[carNumber].incrementAndGet();
                        } while (true);
                    }

                    System.out.println("Done!");
                } catch (IOException e) {
                    System.err.println("Worker thread threw IOException:\n");
                    e.printStackTrace();
                }
            });

            updateTimer.start();
        }
    }
}
