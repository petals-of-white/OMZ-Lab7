import sys
import os
import pydicom
import numpy as np
from PyQt5.QtWidgets import QApplication, QMainWindow, QFileDialog,
QVBoxLayout, QWidget, QPushButton, QSlider, QDockWidget
from PyQt5.QtCore import Qt
from OpenGL.GL import *
from OpenGL.GLUT import *
from OpenGL.GLU import *
from PyQt5.QtOpenGL import QGLWidget
class Dicom3DViewer(QGLWidget):
def __init__(self, parent=None):
super(Dicom3DViewer, self).__init__(parent)
self.volume = None
self.texture_id = None
self.translation_yz_angle = 0
self.original_volume = None
self.scale_factor = 1.0
def load_dicom_series(self, directory):
files = [os.path.join(directory, f) for f in
os.listdir(directory) if f.endswith('.dcm')]
files.sort()
slices = [pydicom.dcmread(f) for f in files]
slices.sort(key=lambda s: s.InstanceNumber)
volume = np.stack([s.pixel_array for s in slices], axis=0)
volume = volume.astype(np.float32)
volume = self.normalize_image(volume)
self.volume = volume
self.depth, self.width, self.height = volume.shape
self.original_volume = volume.copy()
print(f'Loaded volume with dimensions:
{self.depth}x{self.width}x{self.height}')
def normalize_image(self, image):
min_val = np.min(image)
max_val = np.max(image)
print(f"Image min: {min_val}, max: {max_val}")
return np.uint16((image - min_val) / (max_val - min_val) *
65535)
def initializeGL(self):
glEnable(GL_DEPTH_TEST)
self.init_texture()
def resizeGL(self, w, h):
glViewport(0, 0, w, h)
glMatrixMode(GL_PROJECTION)
glLoadIdentity()
glOrtho(-1, 1, -1, 1, -2, 8)
glMatrixMode(GL_MODELVIEW)
def paintGL(self):
glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
glLoadIdentity()
gluLookAt(1.0, 1.0, 2.0, 0.5, 0.5, 0, 0, 1, 0)
self.draw_axes()
self.draw_volume()
def init_texture(self):
if self.volume is not None:
if self.texture_id:
glDeleteTextures([self.texture_id])
self.texture_id = glGenTextures(1)
glBindTexture(GL_TEXTURE_3D, self.texture_id)
print(f"Binding texture with dimensions:
{self.width}x{self.height}x{self.depth}")
glTexImage3D(GL_TEXTURE_3D, 0, GL_LUMINANCE, self.width,
self.height, self.depth, 0, GL_LUMINANCE, GL_UNSIGNED_SHORT, self.volume)
glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MIN_FILTER, GL_LINEAR)
glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_S,
GL_CLAMP_TO_EDGE)
glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_T,
GL_CLAMP_TO_EDGE)
glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_R,
GL_CLAMP_TO_EDGE)
glBindTexture(GL_TEXTURE_3D, 0)
def draw_axes(self):
glBegin(GL_LINES)
glColor3f(1, 0, 0)
glVertex3f(0, 0, 0)
glVertex3f(1, 0, 0)
glColor3f(0, 1, 0)
glVertex3f(0, 0, 0)
glVertex3f(0, 1, 0)
glColor3f(0, 0, 1)
glVertex3f(0, 0, 0)
glVertex3f(0, 0, 1)
glEnd()
def draw_volume(self):
if self.texture_id is None:
print("No texture to draw.")
return
glPushMatrix()
glTranslatef(0.5, 0.5, 0.5)
glScalef(self.scale_factor, self.scale_factor,
self.scale_factor)
glRotatef(self.translation_yz_angle, 1, 0, 0)
glTranslatef(-0.5, -0.5, -0.5)
glEnable(GL_TEXTURE_3D)
glBindTexture(GL_TEXTURE_3D, self.texture_id)
glBegin(GL_QUADS)
glColor3f(1, 1, 1)
for z in range(self.depth):
z_coord = z / self.depth
glTexCoord3f(0, 0, z_coord)
glVertex3f(-0.5, -0.5, z_coord - 0.5)
glTexCoord3f(1, 0, z_coord)
glVertex3f(0.5, -0.5, z_coord - 0.5)
glTexCoord3f(1, 1, z_coord)
glVertex3f(0.5, 0.5, z_coord - 0.5)
glTexCoord3f(0, 1, z_coord)
glVertex3f(-0.5, 0.5, z_coord - 0.5)
glEnd()
glDisable(GL_TEXTURE_3D)
glPopMatrix()
print(f"Volume drawn with angle {self.translation_yz_angle} and
scale {self.scale_factor}")
def set_translation_yz_angle(self, angle):
self.translation_yz_angle = angle
self.update()
def set_scale_factor(self, scale):
self.scale_factor = scale
self.update()
def restore_original_volume(self):
if self.original_volume is not None:
self.volume = self.original_volume.copy()
self.init_texture()
self.update()
class MainWindow(QMainWindow):
def __init__(self):
super(MainWindow, self).__init__()
self.viewer = Dicom3DViewer(self)
self.setCentralWidget(self.viewer)
self.initUI()
def initUI(self):
self.setWindowTitle('3D DICOM Viewer')
self.setGeometry(100, 100, 1400, 1000)
openButton = QPushButton('Open DICOM Series', self)
openButton.clicked.connect(self.open_dicom_series)
openButton.setFixedSize(200, 30)
slider = QSlider(Qt.Horizontal, self)
slider.setMinimum(-180)
slider.setMaximum(180)
slider.setValue(0)
slider.setTickPosition(QSlider.TicksBelow)
slider.setTickInterval(10)
slider.valueChanged.connect(self.slider_changed)
scaleSlider = QSlider(Qt.Horizontal, self)
scaleSlider.setMinimum(1)
scaleSlider.setMaximum(300)
scaleSlider.setValue(100)
scaleSlider.setTickPosition(QSlider.TicksBelow)
scaleSlider.setTickInterval(10)
scaleSlider.valueChanged.connect(self.scale_slider_changed)
layout = QVBoxLayout()
layout.addWidget(openButton)
layout.addWidget(slider)
layout.addWidget(scaleSlider)
container = QWidget()
container.setLayout(layout)
dockWidget = QDockWidget("Controls", self)
dockWidget.setWidget(container)
self.addDockWidget(Qt.RightDockWidgetArea, dockWidget)
def open_dicom_series(self):
directory = QFileDialog.getExistingDirectory(self, "Select DICOM
Directory")
if directory:
self.viewer.load_dicom_series(directory)
self.viewer.init_texture()
self.viewer.update()
def restore_original_volume(self):
self.viewer.restore_original_volume()
def slider_changed(self, value):
self.viewer.set_translation_yz_angle(value)
def scale_slider_changed(self, value):
scale_factor = value / 100.0
self.viewer.set_scale_factor(scale_factor)
if __name__ == '__main__':
app = QApplication(sys.argv)
mainWin = MainWindow()
mainWin.show()
sys.exit(app.exec_())